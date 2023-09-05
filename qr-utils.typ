

#import "bits.typ"
#import "ecc.typ"


#let mod = calc.rem
#let mod2(x) = calc.rem(x, 2)
#let mod3(x) = calc.rem(x, 3)
#let mod255(x) = calc.rem(x, 255)
#let mod256(x) = calc.rem(x, 256)
#let mod285(x) = calc.rem(x, 285)


#let check-version(version) = version >= 1 and version <= 40

#let check-ecl(ecl) = ecl in ("l", "m", "q", "h")

#let get-size(version) = { return 21 + (version - 1)*4 }

#let char-count-indicator(characters, version, mode) = {
  let len
  if version <= 9 {
    len = (10, 9, 8, 8).at(mode)
  } else if version <= 26 {
    len = (12, 11, 16, 10).at(mode)
  } else {
    len = (14, 13, 16, 12).at(mode)
  }
  return bits.pad(
    bits.from-int(characters.len()), len)
}

#let alignment-positions(version) = {
  return (
    (), (6,18), (6, 22), (6, 26), (6, 30), (6, 34),
    (6, 22, 38), (6, 24, 42), (6, 26, 46),
    (6, 28, 50), (6, 30, 54), (6, 32, 58),
    (6, 34, 62), (6, 26, 46, 66), (6, 26, 48, 70),
    (6, 26, 50, 74), (6, 30, 54, 78), (6, 30, 56, 82),
    (6, 30, 58, 86), (6, 34, 62, 90), (6, 28, 50, 72, 94),
    (6, 26, 50, 74, 98), (6, 30, 54, 78, 102), (6, 28, 54, 80, 106),
    (6, 32, 58, 84, 110), (6, 30, 58, 86, 114),
    (6, 34, 62, 90, 118), (6, 26, 50, 74, 98, 122),
    (6, 30, 54, 78, 102, 126), (6, 26, 52, 78, 104, 130),
    (6, 30, 56, 82, 108, 134), (6, 34, 60, 86, 112, 138),
    (6, 30, 58, 86, 114, 142), (6, 34, 62, 90, 118, 146),
    (6, 30, 54, 78, 102, 126, 150),
    (6, 24, 50, 76, 102, 128, 154),
    (6, 28, 54, 80, 106, 132, 158),
    (6, 32, 58, 84, 110, 136, 162),
    (6, 26, 54, 82, 110, 138, 166),
    (6, 30, 58, 86, 114, 142, 170)
  ).at(version - 1)
}

#let is-valid-alignment(x, y, size) = not((x < 8 and y < 8) or (x < 8 and y > size - 8) or (x > size - 8 and y < 8))


#let is-reserved(i, j, version) = {
  let size = get-size(version)

  // timing patterns + black module
  if (i == 6 or j == 6) or (i == size - 8 and j == 8) {
    return true

  // finder patterns + spacing + format information
  } else if ((i < 9 or i > size - 9) and j < 9) or (i < 9 and j > size - 9) {
      return true

  // version information
  } else if version > 6 {
    if ((i == size - 10 or i == size - 9) and j < 6) or ((j == size - 10 or j == size - 9) and i < 6) {
      return true
    }
  }

  // Alignment patterns
  let a = alignment-positions(version)
  for x in a {
    for y in a {
      if is-valid-alignment(x, y, size) {
        // // Check pattern
        if j < x - 2 {
          return false
        } else if j < x + 3 and i < y - 2 {
          return false
        } else if j < x + 3 and i < y + 3 {
          return true
        }
      }
    }
  }
  return false
}


// =================================
//  Templates
// =================================
#let finder = (
  "1111111",
  "1000001",
  "1011101",
  "1011101",
  "1011101",
  "1000001",
  "1111111"
).map(bits.from-str)

#let alignment = (
  "11111",
  "10001",
  "10101",
  "10001",
  "11111"
).map(bits.from-str)


// =================================
//  Encoding modes
// =================================

/// Encodes a numeric character into codewords
#let encode-numeric(nums) = {
  let code = ()
  // for i in range(int(calc.ceil(nums.len()/3))) {
  //   let n = int(nums.slice(i*3, calc.min(nums.len(), i*3 + 3)))
  //   code += bits.from-int(n)
  // }
  let len = nums.len()
  for i in range(len, step:3) {
    let n = int(nums.slice(i, calc.min(len, i + 3)))
    // code += bits.from-int(n)
    if n < 10 {
      code += bits.from-int(n, pad:4)
    } else if n < 100 {
      code += bits.from-int(n, pad:7)
    } else {
      code += bits.from-int(n, pad:10)
    }
  }
  return code
}

#let alphnum-char-codes = (
  "0": 0, "1": 1, "2": 2, "3": 3, "4": 4,
  "5": 5, "6": 6, "7": 7, "8": 8, "9": 9,
  "A": 10, "B": 11, "C": 12, "D": 13, "E": 14,
  "F": 15, "G": 16, "H": 17, "I": 18, "J": 19,
  "K": 20, "L": 21, "M": 22, "N": 23, "O": 24,
  "P": 25, "Q": 26, "R": 27, "S": 28, "T": 29,
  "U": 30, "V": 31, "W": 32, "X": 33, "Y": 34,
  "Z": 35, " ": 36, "$": 37, "%": 38, "*": 39,
  "+": 40, "-": 41, ".": 42, "/": 43, ":": 44,
)

#let encode-alphanumeric(alphnum) = {
  let code = ()
  for i in range(int(calc.ceil(alphnum.len()/2))) {
    let char = alphnum.at(i*2)
    let char-code = alphnum-char-codes.at(char, default:0)

    if i*2 + 1 < alphnum.len() {
      char = alphnum.at(i*2 + 1)
      char-code = char-code*45 + alphnum-char-codes.at(char, default:0)
      code += bits.pad(bits.from-int(char-code), 11)
    } else {
      code += bits.pad(bits.from-int(char-code), 6)
    }
  }
  return code
}

#let ASCII = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

#let encode-byte(chars) = {
  return chars.codepoints().map(c => 32 + ASCII.position(c)).map(bits.from-int.with(pad:8)).flatten()
}

#let get-encoder(mode) = {
  return (
    encode-numeric,
    encode-alphanumeric,
    encode-byte,
    none
  ).at(mode)
}


#let get-mode(data) = {
  let nums = regex(`^\d*$`.text)
  let alphnum = regex(`^[\dA-Z $%*+\-./:]*$`.text)
  let byte = regex(`^[\x00-\xff]*$`.text)
  if data.match(nums) != none {
    return 0
  }
  if data.match(alphnum) != none {
    return 1
  }
  if data.match(byte) != none {
    return 2
  }
  return none
}

#let mode-inidcator(mode) = {
  return (
    (false, false, false, true),
    (false, false, true, false),
    (false, true, false, false),
    (true, false, false, false)
  ).at(mode)
}


// =================================
//  Masking
// =================================
#let masks = (
  (i, j) => mod2(i+j) == 0,
  (i, j) => mod2(i) == 0,
  (i, j) => mod3(j) == 0,
  (i, j) => mod3(i+j) == 0,
  (i, j) => mod2(int(i / 2) + int(j / 3)) == 0,
  (i, j) => (mod2(i * j) + mod3(i * j)) == 0,
  (i, j) => mod2(mod2(i * j) + mod3(i * j)) == 0,
  (i, j) => mod2(mod2(i + j) + mod3(i * j)) == 0
)

#let is-masked(i, j, mask) = (masks.at(mask))(i, j)

#let apply-mask(i, j, bit, mask) = is-masked(i, j, mask) != bit

#let check-mask(field, mask, version) = {
  let size = get-size(version)

  let mask-count = masks.len()

  let penalties = (0,0,0,0)
  let cond1-runs = (0, 0)
  let cond2-win = ()
  let cond3-pattern1 = bits.from-str("00001011101")
  let cond3-pattern2 = bits.from-str("10111010000")
  let cond4-n = 0

  for i in range(size) {
    let cond3-win = ((), ())

    for j in range(size) {
      let bits = (field.at(i).at(j),  field.at(j).at(i))
      let masked-bits = bits
      if not is-reserved(i, j, version) {
        masked-bits.at(0) = apply-mask(i, j, masked-bits.at(0), mask)
      }
      if not is-reserved(j, i, version) {
        masked-bits.at(1) = apply-mask(j, i, masked-bits.at(1), mask)
      }

      // Condition 1
      // Check rows and cols for runs of 5 or more
      // modules of same value
      for x in (0, 1) {
        if masked-bits.at(x) {
          cond1-runs.at(x) += 1
        } else {
          if cond1-runs.at(x) >= 5 {
            penalties.at(0) += 3 + calc.max(0,
              (cond1-runs.at(x) - 5))
            cond1-runs.at(x) = 0
          }
        }
      }

      // Condition 2
      // Use a running window of 2x2 modules and
      // with (i,j) in the top right and check for
      // the same value.
      if i > 0 and j > 0 {
        if cond2-win.len() < 4 {
          cond2-win = (
            masked-bits.at(0),
            apply-mask(i - 1, j, field.at(i - 1).at(j  ), mask),
            apply-mask(i, j - 1, field.at(i  ).at(j - 1), mask),
            apply-mask(i - 1, j - 1, field.at(i - 1).at(j - 1), mask)
          )
        } else {
          cond2-win = (
            masked-bits.at(0),
            apply-mask(i - 1, j, field.at(i - 1).at(j), mask),
            cond2-win.at(0),
            cond2-win.at(1)
          )
        }
        // Check for blocks of same color
        if cond2-win.all(x => x==true) or cond2-win.all(x => x==false) {
          penalties.at(1) += 3
        }
      }

      // Condition 3
      // Use running windows for rows and columns
      // to check against the predefined patterns.
      for x in (0, 1) {
        if cond3-win.at(x).len() < 11 {
          cond3-win.at(x).push( masked-bits.at(x) )
        } else {
          cond3-win.at(x) = cond3-win.at(x).slice(1) + (masked-bits.at(x),)
        }
        if cond3-win.at(x) == cond3-pattern1 or cond3-win.at(x) == cond3-pattern2 {
          penalties.at(2) += 40
        }
      }

      // Condition 4
      // Just count black modules for now
      if masked-bits.at(0) {
        cond4-n += 1
      }
    }
  }
  // Condition 4
  // compute final penalty
  let total = size * size
  let p = int((cond4-n / total) * 100)
  let v = (p - calc.rem(p, 5), p + (5 - calc.rem(p, 5)))
  v = v.map(x => calc.quo(calc.abs(x - 50), 5))
  penalties.at(3) = calc.min(..v) * 10

  // calculate sum of condition 1 to 4 panalties
  return penalties.fold(0, (s, x) => s + x)
}

#let get-best-mask(field, version) = {
  let mask = 0
  let penalty = check-mask(field, 0, version)
  for m in range(1, masks.len()) {
    let p = check-mask(field, m, version)
    if p < penalty {
      mask = m
      penalty = p
    }
  }
  return mask
}


// =================================
//  Version format lookup table
// =================================
// For version > 7 get the format code at index version - 1
#let version-fmt-lut = (
  "000111110010010100",
  "001000010110111100",
  "001001101010011001",
  "001010010011010011",
  "001011101111110110",
  "001100011101100010",
  "001101100001000111",
  "001110011000001101",
  "001111100100101000",
  "010000101101111000",
  "010001010001011101",
  "010010101000010111",
  "010011010100110010",
  "010100100110100110",
  "010101011010000011",
  "010110100011001001",
  "010111011111101100",
  "011000111011000100",
  "011001000111100001",
  "011010111110101011",
  "011011000010001110",
  "011100110000011010",
  "011101001100111111",
  "011110110101110101",
  "011111001001010000",
  "100000100111010101",
  "100001011011110000",
  "100010100010111010",
  "100011011110011111",
  "100100101100001011",
  "100101010000101110",
  "100110101001100100",
  "100111010101000001",
  "101000110001101001"
)

#let ver-fmt(version) = {
  if version >= 7 {
    return bits.from-str(version-fmt-lut.at(version - 7))
  } else {
    return ()
  }
}

// Manual calculation of version format code
#let calc-ver-fmt(version) = {
  if version >= 7 {
    let ver-fmt = bits.pad(
      bits.from-int(version), 6)
    return ver-fmt + ecc.bch(ver-fmt,
      generator:"1111100100101")
  } else {
    return ()
  }
}

// =================================
//  Format codes lookup table
// =================================
// Keys are "ecl-mask"
#let format-lut = (
  "l-0": "111011111000100",
  "l-1": "111001011110011",
  "l-2": "111110110101010",
  "l-3": "111100010011101",
  "l-4": "110011000101111",
  "l-5": "110001100011000",
  "l-6": "110110001000001",
  "l-7": "110100101110110",
  "m-0": "101010000010010",
  "m-1": "101000100100101",
  "m-2": "101111001111100",
  "m-3": "101101101001011",
  "m-4": "100010111111001",
  "m-5": "100000011001110",
  "m-6": "100111110010111",
  "m-7": "100101010100000",
  "q-0": "011010101011111",
  "q-1": "011000001101000",
  "q-2": "011111100110001",
  "q-3": "011101000000110",
  "q-4": "010010010110100",
  "q-5": "010000110000011",
  "q-6": "010111011011010",
  "q-7": "010101111101101",
  "h-0": "001011010001001",
  "h-1": "001001110111110",
  "h-2": "001110011100111",
  "h-3": "001100111010000",
  "h-4": "000011101100010",
  "h-5": "000001001010101",
  "h-6": "000110100001100",
  "h-7": "000100000111011"
)

#let ecl-mask-fmt(ecl, mask) = {
  if type(ecl) == "integer" {
    ecl = ("l", "m", "q", "h").at(ecl)
  }
  let key = (ecl, str(mask)).join("-")
  return bits.from-str(format-lut.at(key))
}

// Manual calculation of format code
#let calc-ecl-mask-fmt(ecl, mask) = {
  if type(ecl) == "integer" {
    ecl = ("l", "m", "q", "h").at(ecl)
  }

  let mask-bits = bits.pad(bits.from-int(mask), 3)
  ecl = (
    l:(false, true),
    m:(false, false),
    q:(true, true),
    h:(true, false)
  ).at(ecl)

  let fmt = ecl + mask-no
  let crc = ecc.bch(fmt)

  return bits.xor(fmt + crc,
    bits.from-str("101010000010010"))
}


// =================================
//  Code capacity lookup table
// =================================
// Keys are "version-ecl"
// Modes are 0:Numerical, 1:Alphanumerical, 2:Byte, 3:Kanji
#let capacities = (
  "1-l": (41, 25, 17, 10),
  "1-m": (34, 20, 14, 8),
  "1-q": (27, 16, 11, 7),
  "1-h": (17, 10, 7, 4),
  "2-l": (77, 47, 32, 20),
  "2-m": (63, 38, 26, 16),
  "2-q": (48, 29, 20, 12),
  "2-h": (34, 20, 14, 8),
  "3-l": (127, 77, 53, 32),
  "3-m": (101, 61, 42, 26),
  "3-q": (77, 47, 32, 20),
  "3-h": (58, 35, 24, 15),
  "4-l": (187, 114, 78, 48),
  "4-m": (149, 90, 62, 38),
  "4-q": (111, 67, 46, 28),
  "4-h": (82, 50, 34, 21),
  "5-l": (255, 154, 106, 65),
  "5-m": (202, 122, 84, 52),
  "5-q": (144, 87, 60, 37),
  "5-h": (106, 64, 44, 27),
  "6-l": (322, 195, 134, 82),
  "6-m": (255, 154, 106, 65),
  "6-q": (178, 108, 74, 45),
  "6-h": (139, 84, 58, 36),
  "7-l": (370, 224, 154, 95),
  "7-m": (293, 178, 122, 75),
  "7-q": (207, 125, 86, 53),
  "7-h": (154, 93, 64, 39),
  "8-l": (461, 279, 192, 118),
  "8-m": (365, 221, 152, 93),
  "8-q": (259, 157, 108, 66),
  "8-h": (202, 122, 84, 52),
  "9-l": (552, 335, 230, 141),
  "9-m": (432, 262, 180, 111),
  "9-q": (312, 189, 130, 80),
  "9-h": (235, 143, 98, 60),
  "10-l": (652, 395, 271, 167),
  "10-m": (513, 311, 213, 131),
  "10-q": (364, 221, 151, 93),
  "10-h": (288, 174, 119, 74),
  "11-l": (772, 468, 321, 198),
  "11-m": (604, 366, 251, 155),
  "11-q": (427, 259, 177, 109),
  "11-h": (331, 200, 137, 85),
  "12-l": (883, 535, 367, 226),
  "12-m": (691, 419, 287, 177),
  "12-q": (489, 296, 203, 125),
  "12-h": (374, 227, 155, 96),
  "13-l": (1022, 619, 425, 262),
  "13-m": (796, 483, 331, 204),
  "13-q": (580, 352, 241, 149),
  "13-h": (427, 259, 177, 109),
  "14-l": (1101, 667, 458, 282),
  "14-m": (871, 528, 362, 223),
  "14-q": (621, 376, 258, 159),
  "14-h": (468, 283, 194, 120),
  "15-l": (1250, 758, 520, 320),
  "15-m": (991, 600, 412, 254),
  "15-q": (703, 426, 292, 180),
  "15-h": (530, 321, 220, 136),
  "16-l": (1408, 854, 586, 361),
  "16-m": (1082, 656, 450, 277),
  "16-q": (775, 470, 322, 198),
  "16-h": (602, 365, 250, 154),
  "17-l": (1548, 938, 644, 397),
  "17-m": (1212, 734, 504, 310),
  "17-q": (876, 531, 364, 224),
  "17-h": (674, 408, 280, 173),
  "18-l": (1725, 1046, 718, 442),
  "18-m": (1346, 816, 560, 345),
  "18-q": (948, 574, 394, 243),
  "18-h": (746, 452, 310, 191),
  "19-l": (1903, 1153, 792, 488),
  "19-m": (1500, 909, 624, 384),
  "19-q": (1063, 644, 442, 272),
  "19-h": (813, 493, 338, 208),
  "20-l": (2061, 1249, 858, 528),
  "20-m": (1600, 970, 666, 410),
  "20-q": (1159, 702, 482, 297),
  "20-h": (919, 557, 382, 235),
  "21-l": (2232, 1352, 929, 572),
  "21-m": (1708, 1035, 711, 438),
  "21-q": (1224, 742, 509, 314),
  "21-h": (969, 587, 403, 248),
  "22-l": (2409, 1460, 1003, 618),
  "22-m": (1872, 1134, 779, 480),
  "22-q": (1358, 823, 565, 348),
  "22-h": (1056, 640, 439, 270),
  "23-l": (2620, 1588, 1091, 672),
  "23-m": (2059, 1248, 857, 528),
  "23-q": (1468, 890, 611, 376),
  "23-h": (1108, 672, 461, 284),
  "24-l": (2812, 1704, 1171, 721),
  "24-m": (2188, 1326, 911, 561),
  "24-q": (1588, 963, 661, 407),
  "24-h": (1228, 744, 511, 315),
  "25-l": (3057, 1853, 1273, 784),
  "25-m": (2395, 1451, 997, 614),
  "25-q": (1718, 1041, 715, 440),
  "25-h": (1286, 779, 535, 330),
  "26-l": (3283, 1990, 1367, 842),
  "26-m": (2544, 1542, 1059, 652),
  "26-q": (1804, 1094, 751, 462),
  "26-h": (1425, 864, 593, 365),
  "27-l": (3517, 2132, 1465, 902),
  "27-m": (2701, 1637, 1125, 692),
  "27-q": (1933, 1172, 805, 496),
  "27-h": (1501, 910, 625, 385),
  "28-l": (3669, 2223, 1528, 940),
  "28-m": (2857, 1732, 1190, 732),
  "28-q": (2085, 1263, 868, 534),
  "28-h": (1581, 958, 658, 405),
  "29-l": (3909, 2369, 1628, 1002),
  "29-m": (3035, 1839, 1264, 778),
  "29-q": (2181, 1322, 908, 559),
  "29-h": (1677, 1016, 698, 430),
  "30-l": (4158, 2520, 1732, 1066),
  "30-m": (3289, 1994, 1370, 843),
  "30-q": (2358, 1429, 982, 604),
  "30-h": (1782, 1080, 742, 457),
  "31-l": (4417, 2677, 1840, 1132),
  "31-m": (3486, 2113, 1452, 894),
  "31-q": (2473, 1499, 1030, 634),
  "31-h": (1897, 1150, 790, 486),
  "32-l": (4686, 2840, 1952, 1201),
  "32-m": (3693, 2238, 1538, 947),
  "32-q": (2670, 1618, 1112, 684),
  "32-h": (2022, 1226, 842, 518),
  "33-l": (4965, 3009, 2068, 1273),
  "33-m": (3909, 2369, 1628, 1002),
  "33-q": (2805, 1700, 1168, 719),
  "33-h": (2157, 1307, 898, 553),
  "34-l": (5253, 3183, 2188, 1347),
  "34-m": (4134, 2506, 1722, 1060),
  "34-q": (2949, 1787, 1228, 756),
  "34-h": (2301, 1394, 958, 590),
  "35-l": (5529, 3351, 2303, 1417),
  "35-m": (4343, 2632, 1809, 1113),
  "35-q": (3081, 1867, 1283, 790),
  "35-h": (2361, 1431, 983, 605),
  "36-l": (5836, 3537, 2431, 1496),
  "36-m": (4588, 2780, 1911, 1176),
  "36-q": (3244, 1966, 1351, 832),
  "36-h": (2524, 1530, 1051, 647),
  "37-l": (6153, 3729, 2563, 1577),
  "37-m": (4775, 2894, 1989, 1224),
  "37-q": (3417, 2071, 1423, 876),
  "37-h": (2625, 1591, 1093, 673),
  "38-l": (6479, 3927, 2699, 1661),
  "38-m": (5039, 3054, 2099, 1292),
  "38-q": (3599, 2181, 1499, 923),
  "38-h": (2735, 1658, 1139, 701),
  "39-l": (6743, 4087, 2809, 1729),
  "39-m": (5313, 3220, 2213, 1362),
  "39-q": (3791, 2298, 1579, 972),
  "39-h": (2927, 1774, 1219, 750),
  "40-l": (7089, 4296, 2953, 1817),
  "40-m": (5596, 3391, 2331, 1435),
  "40-q": (3993, 2420, 1663, 1024),
  "40-h": (3057, 1852, 1273, 784)
)

#let get-capacities(version, ecl) = {
  let key = str(version) + "-" + ecl
  return capacities.at(key)
}

#let find-min-version(characters, ecl, mode) = {
  for ver in range(40) {
    let cap = get-capacities(ver + 1, ecl)
    if cap.at(mode) >= characters {
      return ver + 1
    }
  }
  return -1
}


// =================================
//  ECL lookup table
// =================================
#let ecl-lut = (
  "1-l": (19, 7, 1, 19),
  "1-m": (16, 10, 1, 16),
  "1-q": (13, 13, 1, 13),
  "1-h": (9, 17, 1, 9),
  "2-l": (34, 10, 1, 34),
  "2-m": (28, 16, 1, 28),
  "2-q": (22, 22, 1, 22),
  "2-h": (16, 28, 1, 16),
  "3-l": (55, 15, 1, 55),
  "3-m": (44, 26, 1, 44),
  "3-q": (34, 18, 2, 17),
  "3-h": (26, 22, 2, 13),
  "4-l": (80, 20, 1, 80),
  "4-m": (64, 18, 2, 32),
  "4-q": (48, 26, 2, 24),
  "4-h": (36, 16, 4, 9),
  "5-l": (108, 26, 1, 108),
  "5-m": (86, 24, 2, 43),
  "5-q": (62, 18, 2, 15, 2, 16),
  "5-h": (46, 22, 2, 11, 2, 12),
  "6-l": (136, 18, 2, 68),
  "6-m": (108, 16, 4, 27),
  "6-q": (76, 24, 4, 19),
  "6-h": (60, 28, 4, 15),
  "7-l": (156, 20, 2, 78),
  "7-m": (124, 18, 4, 31),
  "7-q": (88, 18, 2, 14, 4, 15),
  "7-h": (66, 26, 4, 13, 1, 14),
  "8-l": (194, 24, 2, 97),
  "8-m": (154, 22, 2, 38, 2, 39),
  "8-q": (110, 22, 4, 18, 2, 19),
  "8-h": (86, 26, 4, 14, 2, 15),
  "9-l": (232, 30, 2, 116),
  "9-m": (182, 22, 3, 36, 2, 37),
  "9-q": (132, 20, 4, 16, 4, 17),
  "9-h": (100, 24, 4, 12, 4, 13),
  "10-l": (274, 18, 2, 68, 2, 69),
  "10-m": (216, 26, 4, 43, 1, 44),
  "10-q": (154, 24, 6, 19, 2, 20),
  "10-h": (122, 28, 6, 15, 2, 16),
  "11-l": (324, 20, 4, 81),
  "11-m": (254, 30, 1, 50, 4, 51),
  "11-q": (180, 28, 4, 22, 4, 23),
  "11-h": (140, 24, 3, 12, 8, 13),
  "12-l": (370, 24, 2, 92, 2, 93),
  "12-m": (290, 22, 6, 36, 2, 37),
  "12-q": (206, 26, 4, 20, 6, 21),
  "12-h": (158, 28, 7, 14, 4, 15),
  "13-l": (428, 26, 4, 107),
  "13-m": (334, 22, 8, 37, 1, 38),
  "13-q": (244, 24, 8, 20, 4, 21),
  "13-h": (180, 22, 12, 11, 4, 12),
  "14-l": (461, 30, 3, 115, 1, 116),
  "14-m": (365, 24, 4, 40, 5, 41),
  "14-q": (261, 20, 11, 16, 5, 17),
  "14-h": (197, 24, 11, 12, 5, 13),
  "15-l": (523, 22, 5, 87, 1, 88),
  "15-m": (415, 24, 5, 41, 5, 42),
  "15-q": (295, 30, 5, 24, 7, 25),
  "15-h": (223, 24, 11, 12, 7, 13),
  "16-l": (589, 24, 5, 98, 1, 99),
  "16-m": (453, 28, 7, 45, 3, 46),
  "16-q": (325, 24, 15, 19, 2, 20),
  "16-h": (253, 30, 3, 15, 13, 16),
  "17-l": (647, 28, 1, 107, 5, 108),
  "17-m": (507, 28, 10, 46, 1, 47),
  "17-q": (367, 28, 1, 22, 15, 23),
  "17-h": (283, 28, 2, 14, 17, 15),
  "18-l": (721, 30, 5, 120, 1, 121),
  "18-m": (563, 26, 9, 43, 4, 44),
  "18-q": (397, 28, 17, 22, 1, 23),
  "18-h": (313, 28, 2, 14, 19, 15),
  "19-l": (795, 28, 3, 113, 4, 114),
  "19-m": (627, 26, 3, 44, 11, 45),
  "19-q": (445, 26, 17, 21, 4, 22),
  "19-h": (341, 26, 9, 13, 16, 14),
  "20-l": (861, 28, 3, 107, 5, 108),
  "20-m": (669, 26, 3, 41, 13, 42),
  "20-q": (485, 30, 15, 24, 5, 25),
  "20-h": (385, 28, 15, 15, 10, 16),
  "21-l": (932, 28, 4, 116, 4, 117),
  "21-m": (714, 26, 17, 42),
  "21-q": (512, 28, 17, 22, 6, 23),
  "21-h": (406, 30, 19, 16, 6, 17),
  "22-l": (1006, 28, 2, 111, 7, 112),
  "22-m": (782, 28, 17, 46),
  "22-q": (568, 30, 7, 24, 16, 25),
  "22-h": (442, 24, 34, 13),
  "23-l": (1094, 30, 4, 121, 5, 122),
  "23-m": (860, 28, 4, 47, 14, 48),
  "23-q": (614, 30, 11, 24, 14, 25),
  "23-h": (464, 30, 16, 15, 14, 16),
  "24-l": (1174, 30, 6, 117, 4, 118),
  "24-m": (914, 28, 6, 45, 14, 46),
  "24-q": (664, 30, 11, 24, 16, 25),
  "24-h": (514, 30, 30, 16, 2, 17),
  "25-l": (1276, 26, 8, 106, 4, 107),
  "25-m": (1000, 28, 8, 47, 13, 48),
  "25-q": (718, 30, 7, 24, 22, 25),
  "25-h": (538, 30, 22, 15, 13, 16),
  "26-l": (1370, 28, 10, 114, 2, 115),
  "26-m": (1062, 28, 19, 46, 4, 47),
  "26-q": (754, 28, 28, 22, 6, 23),
  "26-h": (596, 30, 33, 16, 4, 17),
  "27-l": (1468, 30, 8, 122, 4, 123),
  "27-m": (1128, 28, 22, 45, 3, 46),
  "27-q": (808, 30, 8, 23, 26, 24),
  "27-h": (628, 30, 12, 15, 28, 16),
  "28-l": (1531, 30, 3, 117, 10, 118),
  "28-m": (1193, 28, 3, 45, 23, 46),
  "28-q": (871, 30, 4, 24, 31, 25),
  "28-h": (661, 30, 11, 15, 31, 16),
  "29-l": (1631, 30, 7, 116, 7, 117),
  "29-m": (1267, 28, 21, 45, 7, 46),
  "29-q": (911, 30, 1, 23, 37, 24),
  "29-h": (701, 30, 19, 15, 26, 16),
  "30-l": (1735, 30, 5, 115, 10, 116),
  "30-m": (1373, 28, 19, 47, 10, 48),
  "30-q": (985, 30, 15, 24, 25, 25),
  "30-h": (745, 30, 23, 15, 25, 16),
  "31-l": (1843, 30, 13, 115, 3, 116),
  "31-m": (1455, 28, 2, 46, 29, 47),
  "31-q": (1033, 30, 42, 24, 1, 25),
  "31-h": (793, 30, 23, 15, 28, 16),
  "32-l": (1955, 30, 17, 115),
  "32-m": (1541, 28, 10, 46, 23, 47),
  "32-q": (1115, 30, 10, 24, 35, 25),
  "32-h": (845, 30, 19, 15, 35, 16),
  "33-l": (2071, 30, 17, 115, 1, 116),
  "33-m": (1631, 28, 14, 46, 21, 47),
  "33-q": (1171, 30, 29, 24, 19, 25),
  "33-h": (901, 30, 11, 15, 46, 16),
  "34-l": (2191, 30, 13, 115, 6, 116),
  "34-m": (1725, 28, 14, 46, 23, 47),
  "34-q": (1231, 30, 44, 24, 7, 25),
  "34-h": (961, 30, 59, 16, 1, 17),
  "35-l": (2306, 30, 12, 121, 7, 122),
  "35-m": (1812, 28, 12, 47, 26, 48),
  "35-q": (1286, 30, 39, 24, 14, 25),
  "35-h": (986, 30, 22, 15, 41, 16),
  "36-l": (2434, 30, 6, 121, 14, 122),
  "36-m": (1914, 28, 6, 47, 34, 48),
  "36-q": (1354, 30, 46, 24, 10, 25),
  "36-h": (1054, 30, 2, 15, 64, 16),
  "37-l": (2566, 30, 17, 122, 4, 123),
  "37-m": (1992, 28, 29, 46, 14, 47),
  "37-q": (1426, 30, 49, 24, 10, 25),
  "37-h": (1096, 30, 24, 15, 46, 16),
  "38-l": (2702, 30, 4, 122, 18, 123),
  "38-m": (2102, 28, 13, 46, 32, 47),
  "38-q": (1502, 30, 48, 24, 14, 25),
  "38-h": (1142, 30, 42, 15, 32, 16),
  "39-l": (2812, 30, 20, 117, 4, 118),
  "39-m": (2216, 28, 40, 47, 7, 48),
  "39-q": (1582, 30, 43, 24, 22, 25),
  "39-h": (1222, 30, 10, 15, 67, 16),
  "40-l": (2956, 30, 19, 118, 6, 119),
  "40-m": (2334, 28, 18, 47, 31, 48),
  "40-q": (1666, 30, 34, 24, 34, 25),
  "40-h": (1276, 30, 20, 15, 61, 16)
)

#let get-ecl-capacities(version, ecl) = {
  let key = str(version) + "-" + ecl
  return ecl-lut.at(key)
}

#let get-codewords(version, ecl) = {
  let ecl-cap = get-ecl-capacities(version, ecl)
  return ecl-cap.at(0)
}

#let get-ecl-codewords(version, ecl) = {
  let ecl-cap = get-ecl-capacities(version, ecl)
  return ecl-cap.at(1)
}

#let get-ecl-blocks(version, ecl) = {
  let ecl-cap = get-ecl-capacities(version, ecl)
  return ecl-cap.at(2)
}

#let block-sizes(version, ecl) = {
  let ecl-cap = get-ecl-capacities(version, ecl)
  let block-sizes = (ecl-cap.at(3),) * ecl-cap.at(2)
  if ecl-cap.len() > 4 {
    block-sizes += (ecl-cap.at(5),) * ecl-cap.at(4)
  }
  return block-sizes
}

#let ecl-block-count(version, ecl) = {
  let ecl-cap = get-ecl-capacities(version, ecl)
  let count = ecl-cap.at(2)
  if ecl-cap.len() > 4 {
    count += ecl-cap.at(4)
  }
  return count
}

#let cw-per-block(version, ecl, block) = {
  let ecl-cap = get-ecl-capacities(version, ecl)
  if block <= ecl-cap.at(2) {
    return ecl-cap.at(3)
  } else {
    return ecl-cap.at(5)
  }
  return ecl-cap.at(block*2 + 1)
}

#let get-remainders(version) = {
  if version in (2,3,4,5,6) {
    return 7
  } else if version in (14,15,16,17,18,19,20, 28,29,30,3132,33,34) {
    return 3
  } else if version in (21,22,23,24,25,26,27) {
    return 4
  } else {
    return 0
  }
}

#let pad-code(code, version, ecl) = {
  let cw-count = get-codewords(version, ecl)
  let ecl-bits = cw-count * 8

  // Add terminator
  let diff = ecl-bits - code.len()
  code += (false,) * calc.min(diff, 4)

  // Pad with zeros
  let rem = calc.rem(code.len(), 8)
  if rem > 0 {
    code += (false,) * (8 - rem)
  }

  // Pad to capacity
  let quo = calc.quo(code.len(), 8)
  diff = cw-count - quo
  if diff > 0 {
    let cw-pad = bits.from-str("11101100") + bits.from-str("00010001")
    cw-pad = cw-pad * (calc.quo(diff, 2) + 1)
    code += cw-pad.slice(0, diff * 8)
  }

  return code
}


#let exp-lut = (1,
  2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38, 76, 152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192, 157, 39, 78, 156, 37, 74, 148, 53, 106, 212, 181, 119, 238, 193, 159, 35, 70, 140, 5, 10, 20, 40, 80, 160, 93, 186, 105, 210, 185, 111, 222, 161, 95, 190, 97, 194, 153, 47, 94, 188, 101, 202, 137, 15, 30, 60, 120, 240, 253, 231, 211, 187, 107, 214, 177, 127, 254, 225, 223, 163, 91, 182, 113, 226, 217, 175, 67, 134, 17, 34, 68, 136, 13, 26, 52, 104, 208, 189, 103, 206, 129, 31, 62, 124, 248, 237, 199, 147, 59, 118, 236, 197, 151, 51, 102, 204, 133, 23, 46, 92, 184, 109, 218, 169, 79, 158, 33, 66, 132, 21, 42, 84, 168, 77, 154, 41, 82, 164, 85, 170, 73, 146, 57, 114, 228, 213, 183, 115, 230, 209, 191, 99, 198, 145, 63, 126, 252, 229, 215, 179, 123, 246, 241, 255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165, 87, 174, 65, 130, 25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221, 167, 83, 166, 81, 162, 89, 178, 121, 242, 249, 239, 195, 155, 43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244, 245, 247, 243, 251, 235, 203, 139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27, 54, 108, 216, 173, 71, 142, 1
)

#let log-lut = (
  0, // padding
  0, 1, 25, 2, 50, 26, 198, 3, 223, 51, 238, 27, 104, 199, 75, 4, 100, 224, 14, 52, 141, 239, 129, 28, 193, 105, 248, 200, 8, 76, 113, 5, 138, 101, 47, 225, 36, 15, 33, 53, 147, 142, 218, 240, 18, 130, 69, 29, 181, 194, 125, 106, 39, 249, 185, 201, 154, 9, 120, 77, 228, 114, 166, 6, 191, 139, 98, 102, 221, 48, 253, 226, 152, 37, 179, 16, 145, 34, 136, 54, 208, 148, 206, 143, 150, 219, 189, 241, 210, 19, 92, 131, 56, 70, 64, 30, 66, 182, 163, 195, 72, 126, 110, 107, 58, 40, 84, 250, 133, 186, 61, 202, 94, 155, 159, 10, 21, 121, 43, 78, 212, 229, 172, 115, 243, 167, 87, 7, 112, 192, 247, 140, 128, 99, 13, 103, 74, 222, 237, 49, 197, 254, 24, 227, 165, 153, 119, 38, 184, 180, 124, 17, 68, 146, 217, 35, 32, 137, 46, 55, 63, 209, 91, 149, 188, 207, 205, 144, 135, 151, 178, 220, 252, 190, 97, 242, 86, 211, 171, 20, 42, 93, 158, 132, 60, 57, 83, 71, 109, 65, 162, 31, 45, 67, 216, 183, 123, 164, 118, 196, 23, 73, 236, 127, 12, 111, 246, 108, 161, 59, 82, 41, 157, 85, 170, 251, 96, 134, 177, 187, 204, 62, 90, 203, 89, 95, 176, 156, 169, 160, 81, 11, 245, 22, 235, 122, 117, 44, 215, 79, 174, 213, 233, 230, 231, 173, 232, 116, 214, 244, 234, 168, 80, 88, 175
)

#let exp(i) = exp-lut.at(i)

#let log(i) = log-lut.at(i)

// #let gf-add(a, b) = calc.rem(a + b, 256)
// TODO: This is stupid!
#let gf-add(a, b) = {
  return bits.to-int(
    bits.xor(
      bits.from-int(a, pad:8),
      bits.from-int(b, pad:8)
    )
  )
}

#let gf-mul(x, y) = {
  if x==0 or y==0 { return 0 }
  return exp(mod255(log(x) + log(y)))
}

#let gf-div(x,y) = {
  if y == 0 { panic("Division by zero") }
  if x == 0 { return 0 }
  return exp(mod255(log(x) + 255 - log(y)))
  // return exp(mod255(log(x) + log(y) * 254))
}

#let gf-pow(x, power) = {
  return exp(mod255(log(x) * power))
}

#let gf-inv(x) = {
  return exp(255 - log(x))
}

#let gf-poly-scale(p, s) = {
  return p.map(x => gf-mul(x, s))
}

#let gf-ploy-add(p, q) = {
  let (lp, lq, lr) = (p.len(), q.len(), calc.max(p.len(), q.len()))
  let r = (0,) * lr
  for i in range(lp) {
    r.at(i + lr - lp) = p.at(i)
  }
  for i in range(lq) {
    r.at(i + lr - lq) = gf-add(r.at(i + lr - lq), q.at(i))
  }
  return r
}

#let gf-poly-mul(p, q) = {
  let (lp, lq) = (p.len(), q.len())
  let r = (0,) * (lp + lq - 1)

  for i in range(r.len()) {
    for pi in range(i + 1) {
      let qi = i - pi
      if pi < lp and qi < lq {
        r.at(i) = gf-add(
          r.at(i),
          gf-mul(p.at(pi), q.at(qi))
        )
      }
    }
  }

  return r
}

#let gf-poly-eval(p, x) = {
  let y = p.first()
  for i in range(p.len()) {
    y = gf-add(gf-mul(y, x), p.at(i))
  }
  return y
}

#let gf-poly-rem(p, q) = {
  let d = p
  let (lp, lq) = (p.len(), q.len())
  for i in range(lp - lq + 1) {
    let coef = d.at(i)
    if coef != 0 {
      for j in range(1, lq) {
        if q.at(j) != 0 {
          d.at(i + j) = gf-add(
            d.at(i + j),
            gf-mul(q.at(j), coef)
          )
        }
      }
    }
  }
  let sep = -(lq - 1)

  return d.slice(sep)
}

#let rs-generator-lut = (
  (1, 127, 122, 154, 164, 11, 68, 117), (1, 255, 11, 81, 54, 239, 173, 200, 24), (1, 226, 207, 158, 245, 235, 164, 232, 197, 37), (1, 216, 194, 159, 111, 199, 94, 95, 113, 157, 193), (1, 172, 130, 163, 50, 123, 219, 162, 248, 144, 116, 160), (1, 68, 119, 67, 118, 220, 31, 7, 84, 92, 127, 213, 97), (1, 137, 73, 227, 17, 177, 17, 52, 13, 46, 43, 83, 132, 120), (1, 14, 54, 114, 70, 174, 151, 43, 158, 195, 127, 166, 210, 234, 163), (1, 29, 196, 111, 163, 112, 74, 10, 105, 105, 139, 132, 151, 32, 134, 26), (1, 59, 13, 104, 189, 68, 209, 30, 8, 163, 65, 41, 229, 98, 50, 36, 59), (1, 119, 66, 83, 120, 119, 22, 197, 83, 249, 41, 143, 134, 85, 53, 125, 99, 79), (1, 239, 251, 183, 113, 149, 175, 199, 215, 240, 220, 73, 82, 173, 75, 32, 67, 217, 146), (1, 194, 8, 26, 146, 20, 223, 187, 152, 85, 115, 238, 133, 146, 109, 173, 138, 33, 172, 179), (1, 152, 185, 240, 5, 111, 99, 6, 220, 112, 150, 69, 36, 187, 22, 228, 198, 121, 121, 165, 174), (1, 44, 243, 13, 131, 49, 132, 194, 67, 214, 28, 89, 124, 82, 158, 244, 37, 236, 142, 82, 255, 89), (1, 89, 179, 131, 176, 182, 244, 19, 189, 69, 40, 28, 137, 29, 123, 67, 253, 86, 218, 230, 26, 145, 245), (1, 179, 68, 154, 163, 140, 136, 190, 152, 25, 85, 19, 3, 196, 27, 113, 198, 18, 130, 2, 120, 93, 41, 71), (1, 122, 118, 169, 70, 178, 237, 216, 102, 115, 150, 229, 73, 130, 72, 61, 43, 206, 1, 237, 247, 127, 217, 144, 117), (1, 245, 49, 228, 53, 215, 6, 205, 210, 38, 82, 56, 80, 97, 139, 81, 134, 126, 168, 98, 226, 125, 23, 171, 173, 193), (1, 246, 51, 183, 4, 136, 98, 199, 152, 77, 56, 206, 24, 145, 40, 209, 117, 233, 42, 135, 68, 70, 144, 146, 77, 43, 94), (1, 240, 61, 29, 145, 144, 117, 150, 48, 58, 139, 94, 134, 193, 105, 33, 169, 202, 102, 123, 113, 195, 25, 213, 6, 152, 164, 217), (1, 252, 9, 28, 13, 18, 251, 208, 150, 103, 174, 100, 41, 167, 12, 247, 56, 117, 119, 233, 127, 181, 100, 121, 147, 176, 74, 58, 197), (1, 228, 193, 196, 48, 170, 86, 80, 217, 54, 143, 79, 32, 88, 255, 87, 24, 15, 251, 85, 82, 201, 58, 112, 191, 153, 108, 132, 143, 170), (1, 212, 246, 77, 73, 195, 192, 75, 98, 5, 70, 103, 177, 22, 217, 138, 51, 181, 246, 72, 25, 18, 46, 228, 74, 216, 195, 11, 106, 130, 150)
)
#let rs-generator(cw-count) = {
  if cw-count >= 7 and cw-count <= 30 {
    return rs-generator-lut.at(cw-count - 7)
  } else {
    let g = (1,)
    for i in range(cw-count) {
      g = gf-poly-mul(g, (1, exp(i)))
    }
    return g
  }
}

#let rs-codewords(code, version, ecl) = {
  let cw-count = get-ecl-codewords(version, ecl)
  let codewords = ()
  let n = code.len()

  for i in range(n, step:8) {
    codewords.push(bits.to-int(code.slice(i, i + 8)))
  }

  let gen = rs-generator(cw-count)
  let r = gf-poly-rem(codewords + (0,) * (gen.len() - 1), gen)

  return r.map(bits.from-int).map(v => bits.pad(v, 8)).flatten()
}


#let generate-blocks(data, version, ecl) = {
  let block-sizes = block-sizes(version, ecl)
  if block-sizes.len() == 1 {
    return data + rs-codewords(data, version, ecl)
  } else {
    let blocks = ()
    let ec-codewords = ()

    // Get blocks and error correction codewords
    let i = 0
    for bs in block-sizes {
      blocks.push(data.slice(i, i + bs*8))
      ec-codewords.push(rs-codewords(
        blocks.at(-1),
        version, ecl))

      i += bs*8
    }

    // generate interleaved data
    data = ()
    let max = calc.max(..block-sizes) * 8
    for i in range(max, step:8) {
      for b in blocks {
        if i < b.len() {
          data += b.slice(i, i + 8)
        }
      }
    }
    // interleave ec codewords
    max = ec-codewords.at(0).len()
    for i in range(max, step:8) {
      for ec in ec-codewords {
        data += ec.slice(i, i + 8)
      }
    }

    if get-remainders(version) > 0 {
      data += (false,) * get-remainders(version)
    }

    return data
  }
}
