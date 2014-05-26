import Graphics.Input (..)
import Graphics.Input.Field (..)
import Http (..)
import Text

submit = input ""

submitButton = (\text -> button submit.handle text.string "Make Me A Poem") <~ poem.signal

poem = input noContent

poemBox = field { defaultStyle - padding | padding = uniformly 4} poem.handle id "Poem" <~ poem.signal

outputBox = Text.centered <~ (Text.toText <~ (processResponse <~ send (post "/" <~ submit.signal)))

processResponse response = case response of
  Success s -> s
  Waiting -> ""
  Failure i s -> "Fail: " ++ s

main = flow down <~ signals [
  poemBox,
  submitButton,
  outputBox]

return x = (input x).signal

signals s = case s of
  [] -> return []
  (x::xs) -> (::) <~ x ~ signals xs