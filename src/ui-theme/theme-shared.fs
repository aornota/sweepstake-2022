module Aornota.Sweepstake2022.Ui.Theme.Shared

open Aornota.Sweepstake2022.Ui.Theme.Light
open Aornota.Sweepstake2022.Ui.Theme.Dark

let getTheme useDefaultTheme = if useDefaultTheme then themeLight else themeDark
