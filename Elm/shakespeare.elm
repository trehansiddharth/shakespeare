import Graphics.Input (..)
import Graphics.Input.Field (..)
import Graphics.Element (..)
import Http (..)
import Text
import Window

title = Text.centered <| Text.height 32 <| Text.toText "Shakespeare"
subtitle = Text.centered <| Text.height 22 <| Text.toText "Turn Your Text Into A Poem"

submit = input ""

submitButton = (\text -> button submit.handle text.string "Poemify") <~ poem.signal

poem = input noContent

poemBox = field defaultStyle poem.handle id "Input Text" <~ poem.signal

outputBox = Text.centered <~ (Text.toText <~ (processResponse <~ send (post "/" <~ submit.signal)))

processResponse response = case response of
  Success s -> s
  Waiting -> ""
  Failure i s -> "Fail: " ++ s

poemWidth = 500
poemHeight = 120

main = display <~ Window.dimensions ~ (flow down <~ (form <~ Window.dimensions ~ controls))

display (w, h) f = color lightOrange <| container w h middle <| color lightPurple <| container poemWidth (poemHeight + 190) middle <| f

form (w, h) fc = [
  container poemWidth 90 middle header,
  spacer 10 10,
  container poemWidth (poemHeight + 90) middle fc]

header = flow down [
  container poemWidth 45 middle title,
  container poemWidth 45 middle subtitle]

controls = flow down <~ signals [
  container poemWidth 40 middle <~ poemBox,
  container poemWidth 50 middle <~ submitButton,
  container poemWidth poemHeight middle <~ outputBox]

return x = (input x).signal

signals s = case s of
  [] -> return []
  (x::xs) -> (::) <~ x ~ signals xs