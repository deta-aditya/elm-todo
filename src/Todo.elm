module Todo exposing (Todo, ID, id, title, done, appendText, deleteByID, replaceTitleByID, doneByID)

-- TODO

type Todo 
  = Todo Body

type alias Body =
  { id: ID
  , title: String
  , done: Bool
  }

-- GETTERS

id : Todo -> ID
id (Todo todo) =
  todo.id

title : Todo -> String
title (Todo todo) =
  todo.title

done : Todo -> Bool
done (Todo todo) =
  todo.done


-- TRANSFORMERS

finish : Todo -> Todo
finish (Todo todo) =
  Todo { todo | done = True }


-- LIST PROCESSORS

appendText : List Todo -> String -> List Todo
appendText todos text =
  Todo
    { id = nextID todos
    , title = text
    , done = False 
    } 
  :: todos

deleteByID : List Todo -> ID -> List Todo
deleteByID todos idv =
  List.filter (id >> (/=) idv) todos

doneByID : List Todo -> ID -> List Todo
doneByID todos idv =
  todos |>
    List.map (\ todo -> 
      if id todo == idv
        then finish todo
        else todo
    )

replaceTitleByID : List Todo -> String -> ID -> List Todo
replaceTitleByID todos titlev idv =
  todos |>
    List.map (\ (Todo t) -> 
      if t.id == idv
        then Todo { t | title = titlev }
        else Todo t
    )


-- PRIVATE

nextID : List Todo -> ID
nextID todos =
  todos 
    |> List.map id 
    |> List.foldl max starter
    |> increment



-- ID

type ID 
  = ID Int 

starter : ID
starter = ID -1

increment : ID -> ID
increment (ID val) =
  ID (val + 1)

max : ID -> ID -> ID
max (ID id1) (ID id2) =
  if id1 > id2 
    then ID id1
    else ID id2
