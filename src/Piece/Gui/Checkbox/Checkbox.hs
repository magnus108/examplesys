module Piece.Gui.Checkbox.Checkbox (userCheck, CheckboxEntry, entry) where

import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Reactive.Threepenny as R

data CheckboxEntry = CheckboxEntry
  { _elementTE :: UI.Element,
    _userTE :: R.Tidings Bool
  }

instance UI.Widget CheckboxEntry where getElement = _elementTE

userCheck :: CheckboxEntry -> R.Tidings Bool
userCheck = _userTE

entry :: R.Behavior Bool -> UI.UI CheckboxEntry
entry bValue = do
  input <- UI.input UI.# UI.set UI.type_ "checkbox"

  window <- UI.askWindow
  UI.liftIOLater $ R.onChange bValue $ \s -> UI.runUI window $ void $ do
    UI.element input UI.# UI.set UI.checked s

  let _elementTE = input
      _userTE = UI.tidings bValue $ UI.checkedChange input
  return CheckboxEntry {..}
