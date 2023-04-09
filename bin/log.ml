type mode = Silent | Normal | Verbose | Debug

let mode = ref Normal
let log m f =
    if m <= !mode then Format.printf f else Format.ifprintf Format.std_formatter f
let normal f = log Normal f
let debug f = log Debug f
let verbose f = log Verbose f
