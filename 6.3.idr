import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

{-   WHY??????

*6.3> SchemaType (SString .+. (SString .+. SInt))
      (String, String, Int) : Type
*6.3> SchemaType ((SString .+. SString) .+. SInt)
      ((String, String), Int) : Type
-}

{-
data DataStore : Type where
    MkData : (schema : Schema) ->
              (size : Nat) ->
              (items : Vect size (SchemaType schema)) ->
              DataStore

size : DataStore -> Nat
size (MkData schema' size' items') = size'

schema : DataStore -> Schema
schema (MkData schema' size' items') = schema'

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData schema' size' items') = items'
-}

record DataStore where
       constructor MkData
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

data Command : Schema -> Type where
     SetSchema : (newschema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     GetAll : Command schema
     Quit : Command schema

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
  = case xs of
         [] => Just SString
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs)
  = case xs of
         [] => Just SInt
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (SInt .+. xs_sch)
parseSchema ("Char" :: xs)
  = case xs of
         [] => Just SChar
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (SChar .+. xs_sch)

parseSchema _ = Nothing

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem
  = MkData schema _ (addToData store)
    where
      addToData : Vect oldsize (SchemaType schema) ->
                  Vect (S oldsize) (SchemaType schema)
      addToData [] = [newitem]
      addToData (item :: items) = item :: addToData items

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr)
  = "(" ++ (display iteml) ++ ", " ++ (display itemr) ++ ")"

getEntries : (store: DataStore) -> Maybe (String, DataStore)
getEntries store = Just ((foldl (\acc => \elem => acc ++ (display elem) ++ "\n") "" (items store)), store)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("Out of range\n", store)
             Just id => Just (display (index id (items store)) ++ "\n", store)


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SInt input
  = case span isDigit input of
         ("", rest) => Nothing
         (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input
  = case parsePrefix schemal input of
         Nothing => Nothing
         Just (lval, input') =>
           case parsePrefix schemar input' of
             Nothing => Nothing
             Just (rval, input'') => Just ((lval, rval), input'')
parsePrefix SChar input
  = case unpack input of
         (x :: xs) => Just (x, ltrim $ pack xs)
         _ => Nothing

parsePrefix SString input = getQueted (unpack input)
  where
    getQueted : List Char -> Maybe (String, String)
    getQueted ('"' :: xs)
      = case span (/= '"') xs of
         (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
         _ => Nothing
    getQueted _ = Nothing


parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing


parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                    Nothing => Nothing
                                    Just restok => Just (Add restok)

parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val = case all isDigit (unpack val) of
                                    False => Nothing
                                    True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest
  = do sch <- parseSchema (words rest)
       Just $ SetSchema sch
  -- = case parseSchema (words rest) of
  --        Nothing => Nothing
  --        Just sch => Just (SetSchema sch)
parseCommand _ _ _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing

parse : (schema : Schema) ->
        (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
  = case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just (Add item) =>
           Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (SetSchema schema') =>
              case setSchema store schema' of
                   Nothing => Just ("Can't update schema\n", store)
                   Just store' => Just ("Ok\n", store')
         Just (Get pos) => getEntry pos store
         Just GetAll => getEntries store
         Just Quit => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ [])
                "Command: " processInput
