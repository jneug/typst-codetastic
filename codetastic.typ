
#import "@preview/cetz:0.1.0"

#import "bits.typ"
#import "bitfield.typ"
#import "util.typ"
#import "checksum.typ"
#import "ecc.typ"


/// Encode a digit into seven bits according to the
/// EAN-13 standard.
/// Each digit is encoded into seven bits via one
/// of three encoding tables, determined by
/// #arg[i] and #arg[odd].
#let ean13-encode( i, number, odd:none ) = {
  // Get code A codeword
  let code = bits.from-str((
    "0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011"
  ).at(number))

  if i >= 6 {
    // Get code C codewort (right side)
    return bits.inv(code)
  } else if not odd {
    // Get code B codewort (left side, even)
    return bits.inv(code).rev()
  } else {
    // Get code A codewort (left side, odd)
    return code
  }
}

/// Creat an EAN-5 barcode.
#let ean5(
  code,
  scale: 1,
  colors: (white, black)
) = {
  if type(scale) != "array" {
    scale = (scale, scale)
  }
  let (scale-x, scale-y) = (
    calc.clamp(scale.at(0), .8, 2),
    calc.clamp(scale.at(1), .5, 1)
  )

  let (bg, fg) = colors

  code = util.to-int-arr(code)

  let checksum = checksum.ean5(code)
  let parities = bits.from-str((
      "00111", "01011", "01101", "01110", "10011",
      "11001", "11100", "10101", "10110", "11010"
    ).at(checksum))

  // Do the encoding
  let encoded = bits.from-str("01011")
  for (i, n) in code.enumerate() {
    encoded += ean13-encode(i, n, odd:parities.at(i))
    encoded += (false, true)
  }

  // Prepare styles and canvas
  let width = 0.264mm * scale-x
  let height = 18.28mm * scale-x * scale-y
  let font-size = 8pt * scale-x
  cetz.canvas({
    import cetz.draw: set-style, line, rect, content

    util.draw-bars(
      encoded,
      width:width, height:height,
      fg:fg, bg:bg
    )

    content(
      (rel:(0, width), to:"code-bg.top"),
      anchor:"bottom",
      text(
        font: "Arial",
        size: font-size,
        fill: fg,
        code.map(str).join()
      )
    )
  })
}


#let ean8(
  code,
  scale: 1,
  colors: (white, black),
  lmi: false
) = {
    if type(scale) != "array" {
    scale = (scale, scale)
  }
  let (scale-x, scale-y) = (
    calc.clamp(scale.at(0), .8, 2),
    calc.clamp(scale.at(1), .5, 1)
  )

  let (bg, fg) = colors

  code = util.to-int-arr(code)
  if code.len() < 7 {
    code = (0,) * (7 - code.len()) + code
  }
  code = util.check-code(
    code, 8, checksum.ean8, checksum.ean8-test
  )

  // Do the encoding
  let encoded = bits.from-str("00000000000101")
  for (i, n) in code.slice(0,4).enumerate() {
    encoded += ean13-encode(0, n, odd:true)
  }
  encoded += bits.from-str("01010")
  for (i,n) in code.slice(4).enumerate() {
    encoded += ean13-encode(6, n)
  }
  encoded += bits.from-str("1010000000")

  // Prepare styles and canvas
  let width = 0.264mm * scale-x
  let height = 18.28mm * scale-x * scale-y
  let font-size = 6pt * scale-x
  cetz.canvas({
    import cetz.draw: set-style, line, rect, content

    let bg-rect = rect.with(fill:bg, stroke:width + bg)
    let num-content(at, cnt, ..style) = content(
        at,
        anchor:"center",
        text(
          font: "Arial",
          size: font-size, // * 1.18,
          fill: fg,
          cnt
        ),
        ..style
      )

    util.draw-bars(
      encoded,
      width:width, height:height,
      fg:fg, bg:bg
    )

    util.draw-rect((14*width, 0), 28*width, font-size, fill:bg, name:"code-left")
    util.draw-rect((47*width, 0), 28*width, font-size, fill:bg, name:"code-right")

    if lmi {
      num-content(
        (
          rel:(-8.5*width, 0),
          to:"code-left.left"
        ),
        sym.lt
      )
    }
    for (i, n) in code.slice(0, 4).enumerate() {
      num-content(
        (
          rel:((3.5 + i*7) * width, 0),
          to:"code-left.left"
        ),
        str(n)
      )
    }
    for (i, n) in code.slice(4).enumerate() {
      num-content(
        (
          rel:((3.5 + i*7) * width, 0),
          to:"code-right.left"
        ),
        str(n)
      )
    }

    if lmi {
      num-content(
        (
          rel:(6.5*width, 0),
          to:"code-right.right"
        ),
        sym.gt
      )
    }
  })
}


#let ean13(
  code,
  scale: 1,
  colors: (white, black),
  lmi: false
) = {
  if type(scale) != "array" {
    scale = (scale, scale)
  }
  let (scale-x, scale-y) = (
    calc.clamp(scale.at(0), .8, 2),
    calc.clamp(scale.at(1), .5, 1)
  )

  let (bg, fg) = colors

  code = util.to-int-arr(code)
  if code.len() < 12 {
    code = (0,) * (12 - code.len()) + code
  }
  code = util.check-code(
    code, 13, checksum.ean13, checksum.ean13-test
  )
  let first = code.remove(0)

  let parities = bits.from-str((
    "111111", "110100", "110010", "110001", "101100",
    "100110", "100011", "101010", "101001", "100101"
  ).at(first))

  // Do the encoding
  let encoded = bits.from-str("00000000000101")
  for (i, n) in code.slice(0,6).enumerate() {
    encoded += ean13-encode(i, n, odd:parities.at(i))
  }
  encoded += bits.from-str("01010")
  for (i,n) in code.slice(6).enumerate() {
    encoded += ean13-encode(6 + i, n)
  }
  encoded += bits.from-str("1010000000")

  // Prepare styles and canvas
  let width = 0.264mm * scale-x
  let height = 18.28mm * scale-x * scale-y
  let font-size = 6pt * scale-x
  cetz.canvas({
    import cetz.draw: set-style, line, rect, content

    let num-content(at, cnt, ..style) = content(
        at,
        anchor:"center",
        text(
          font: "Arial",
          size: font-size, // * 1.18,
          fill: fg,
          cnt
        ),
        ..style
      )

    util.draw-bars(
      encoded,
      width:width, height:height,
      fg:fg, bg:bg
    )


    util.draw-rect((14*width, 0), 42*width, font-size, name:"code-left")
    util.draw-rect((61*width, 0), 42*width, font-size, name:"code-right")

    num-content(
      (
        rel:(-8.5*width, 0),
        to:"code-left.left"
      ),
      str(first)
    )
    for (i, n) in code.slice(0, 6).enumerate() {
      num-content(
        (
          rel:((3.5 + i*7) * width, 0),
          to:"code-left.left"
        ),
        str(n)
      )
    }
    for (i, n) in code.slice(6).enumerate() {
      num-content(
        (
          rel:((3.5 + i*7) * width, 0),
          to:"code-right.left"
        ),
        str(n)
      )
    }

    if lmi {
      num-content(
        (
          rel:(6.5*width, 0),
          to:"code-right.right"
        ),
        sym.gt
      )
    }
  })
}

#let upc-a(
  code,
  scale: 1,
  colors: (white, black),
  lmi: false
) = {
  if type(scale) != "array" {
    scale = (scale, scale)
  }
  let (scale-x, scale-y) = (
    calc.clamp(scale.at(0), .8, 2),
    calc.clamp(scale.at(1), .5, 1)
  )


  let (bg, fg) = colors

  code = util.to-int-arr(code)
  code = util.check-code(
    code, 12, checksum.upc-a, checksum.upc-a-test
  )

  // Do the encoding
  let encoded = bits.from-str("000000000101")
  for n in code.slice(0,6) {
    encoded += ean13-encode(0, n, odd:true)
  }
  encoded += bits.from-str("01010")
  for n in code.slice(6) {
    encoded += ean13-encode(6, n)
  }
  encoded += bits.from-str("101000000000")

  // Prepare styles and canvas
  let width = 0.33mm * scale-x
  let height = 25.9mm * scale-x * scale-y + 5 * width
  let font-size = 5 * width
  cetz.canvas({
    import cetz.draw: set-style, line, rect, content

    let num-content(at, cnt, ..style) = content(
        at,
        anchor:"center",
        text(
          font: "Arial",
          size: font-size, // * 1.18,
          fill: fg,
          cnt
        ),
        ..style
      )

    util.draw-bars(
      encoded,
      width:width, height:height,
      fg:fg, bg:bg
    )

    util.draw-rect((12*width, 0), 42*width, 5 * width, name:"code-left")
    util.draw-rect((59*width, 0), 42*width, 5 * width, name:"code-right")

    num-content((
        rel:(-7.5*width, 0),
        to:"code-left.left"
      ), str(code.first()))
    for (i, n) in code.slice(1, 6).enumerate() {
      num-content((
          rel:((7 + i*7) * width, 0),
          to:"code-left.left"
        ), str(n))
    }
    for (i, n) in code.slice(6, -1).enumerate() {
      num-content((
          rel:((7 + i*7) * width, 0),
          to:"code-right.left"
        ), str(n))
    }
    num-content((
        rel:(7.5*width, 0),
        to:"code-right.right"
      ), str(code.last()))
  })
}

#let upc-e(
  code,
  scale: 1,
  colors: (white, black),
  lmi: false
) = {}


#let data-matrix( data, size: 14, quiet-zone: 1 ) = {
  // Create matrix with timing and alignment patterns
  let field = bitfield.new(
    14, 14,
    init: (i,j) => {
      i == 0 or j == 0 or (j == 13 and calc.even(i)) or (i == 13 and calc.even(j))
    }
  )

  cetz.canvas({
    util.draw-matrix(field)
  })
}


#let qrcode(
  data,
  quiet-zone: 4,
  min-version: 1,
  ecl: "l",
  mask: auto,
  size: auto,
  width: auto,
  colors: (white, black)
) = {
  import "qr-utils.typ"

  assert(qr-utils.check-ecl(ecl), message:"Error correction level need to be one of l,m,q or h. Got " + repr(ecl))

  let module-size = size
  let (bg, fg) = colors

  // prepare encoding method
  // (numeric, alphanumeric, byte or kanji)
  let mode = qr-utils.get-mode(data)
  if mode == none {
    panic("Supplied data contains characters that can't be encoded with one of the supported methods numeric, alphanumeric or byte (iso-8859-1).")
  }
  let encoder = qr-utils.get-encoder(mode)

  // determine final qr-code version from
  // data size and min-version argument
  min-version = if min-version == auto {1} else {calc.clamp(min-version, 1, 40)}
  let version = calc.max(min-version,
    qr-utils.find-min-version(
      data.len(), ecl, mode)
  )

  // encode data
  let encoded = ()
  encoded += qr-utils.mode-inidcator(mode)
  encoded += qr-utils.char-count-indicator(data, version, mode)
  encoded += encoder(data)
  encoded = qr-utils.pad-code(encoded, version, ecl)

  // interleave data codewords and error correction
  // TODO: implement
  // encoded += eccw
  encoded = qr-utils.generate-blocks(encoded, version, ecl)

  // Create empty matrix
  let size = qr-utils.get-size(version)
  let field = bitfield.new(size, size)

  // Add finder patterns
  for i in range(7) {
    for j in range(7) {
      let v = qr-utils.finder.at(i).at(j)
      field.at(i).at(j) = v
      field.at(i).at(size - j - 1) = v
      field.at(size - i - 1).at(j) = v
    }
  }
  // for pos in ((0, 0), (0, size - 7), (size - 7, 0)) {
  //   field = bitfield.compose(
  //     field, qr-utils.finder,
  //     at:pos
  //   )
  // }

  // Add timing patterns
  for i in range(7, size - 7) {
    field.at(6).at(i) = calc.even(i)
    field.at(i).at(6) = calc.even(i)
  }

  // Add alignment patterns
  let alignment-locations = qr-utils.alignment-positions(version)
  for i in alignment-locations {
    for j in alignment-locations {
      if qr-utils.is-valid-alignment(i, j, size) {
        field = bitfield.compose(
          field, qr-utils.alignment,
          at:(i,j), center:true
        )
      }
    }
  }

  // Add dark module
  field.at(size - 8).at(8) = true

  // Add data in zig-zag pattern
  let d = 0
  let dir = 1
  for j in range(size, 0, step:-2) {
    if j <= 7 { j -= 1 }

    for i in range(size, 0, step:-1) {
      i = if dir == 1 {i - 1} else {size - i}
      for k in (0,1) {
        if d < encoded.len() and not qr-utils.is-reserved(i, j - k - 1, version) {
          field.at(i).at(j - k - 1) = encoded.at(d)
          d += 1
        }
      }
    }
    dir *= -1
  }

  // Masking
  if mask == auto {
    // compute best mask for this code
    mask = qr-utils.get-best-mask(field, version)
  } else if mask != none {
    // use given mask
    mask = calc.clamp(mask, 0, 7)
  }

  // apply mask if not none (for debugging)
  if mask != none {
    for i in range(size) {
      for j in range(size) {
        if not qr-utils.is-reserved(i, j, version) {
          field.at(i).at(j) = qr-utils.apply-mask(
            i, j, field.at(i).at(j), mask
          )
        }
      }
    }
  } else {
    // TODO: Remove!
    // Dummy mask number for adding format information.
    // Just for debugging!
    mask = 0
  }

  // Add format information
  let fmt = qr-utils.ecl-mask-fmt(ecl, mask)
  for (i, b) in fmt.enumerate() {
    if i < 7 {
      field.at(size - i - 1).at(8) = b
      if i > 5 { i+= 1 }
      field.at(8).at(i) = b
    } else {
      field.at(8).at(size - 15 + i) = b
      if i > 8 { i += 1 }
      field.at(15 - i).at(8) = b
    }
  }

  // Adding version information
  if version >= 7 {
    let fmt = qr-utils.ver-fmt(version)

    for i in range(6) {
      field.at(size -  9).at(5 - i) = fmt.at(i*3)
      field.at(size - 10).at(5 - i) = fmt.at(i*3+1)
      field.at(size - 11).at(5 - i) = fmt.at(i*3+2)

      field.at(5 - i).at(size -  9) = fmt.at(i*3)
      field.at(5 - i).at(size - 10) = fmt.at(i*3+1)
      field.at(5 - i).at(size - 11) = fmt.at(i*3+2)
    }
  }

  // calculate module size
  if width != auto {
    module-size = width / size
  } else if module-size == auto {
    // TODO: calculate reasonable module size from version
    module-size = 2mm
  }

  // Draw modules
  cetz.canvas({
    util.draw-matrix(
      field, size:module-size,
      bg:bg, fg:fg,
      quiet-zone: quiet-zone
    )
  })
}
