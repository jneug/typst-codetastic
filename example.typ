
#import "@local/darkmode:0.1.0"
#import "codetastic.typ": *

// #show: darkmode.activate(theme: darkmode.solarized)

// == EAN-13

// #grid(
//   ean13("9099999543217", scale:2),
//   image("assets/ean13_909999954321.png", width:115*0.264mm*2)
// )

// === EAN-13 with EAN-5 supplement
// #grid(
//   columns:2,
//   ean13(909999954321, scale:(2, .8), lmi:true),
//   ean5("54495", scale:(1.6, .8))
// )

// == EAN-8
// #grid(
//   ean8("96385074", scale:2, lmi:true),
//   image("assets/ean8_96385074.png", width:85*0.264mm*2)
// )

// == UPC-A
// #grid(
//   upc-a("90999995432", scale:(2,.8)),
//   image("assets/upc-a_90999995432.gif", width:114*0.33mm*2)
// )

// == Data matrix
//
// #data-matrix(())

== QR-Code

// #qrcode("9928374719093837429394734878")

// #qrcode("HELLO WORLD")

// #qrcode("Hello, World!")

// #let data = lorem(21)
// #grid(
//   columns:2,
//   gutter: 1cm,
//   qrcode(data, size:3mm),
//   data
// )

#import "qr-utils.typ"
#import "bits.typ"
#let data = range(136).map(bits.from-int.with(pad:8)).flatten()
#let blocks = qr-utils.generate-blocks(data, 6, "l")
#let data-blocks = ()
#for i in range(blocks.len() - 8, step:8) {
  data-blocks.push(bits.to-int(blocks.slice(i, i + 8)))
}

#data-blocks
