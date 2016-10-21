module makeBMP

/// simple function for creating a BMP image in a file
/// uses stuff not introduced in course yet, so do not read details


/// creates a file fname.bmp with width w and height h
/// cols : int*int -> int*int*int specifies colours
let makeBMP fname w h cols =
 if w<1 || w>8192 || h<1 || h>8192
 then failwith "Width and height must be between 1 and 8192\n"
 else
  let ofile = new System.IO.BinaryWriter
                   (System.IO.File.Open
                     (fname + ".bmp",
                      System.IO.FileMode.Create,
                      System.IO.FileAccess.Write))
  let w1 = 3*w // width of line of pixels in bytes
  let w3 = (4 - w1%4) % 4
  let w2 = w1 + w3
  let s = 54+w2*h // file size

  let writeWord w =
    ofile.Write(byte (w % 256));
    ofile.Write(byte ((w / 256) % 256));
    ofile.Write(byte ((w / 256 / 256) % 256));
    ofile.Write(byte ((w / 256 / 256 / 256) % 256));

  ofile.Write(byte 66); // B
  ofile.Write(byte 77); // M
  writeWord s;  // size
  writeWord 0;
  writeWord 54; // offset to data
  
  writeWord 40; // size of infoheader
  writeWord w;  // width
  writeWord h;  // height
  ofile.Write(byte 1);  // planes = 1
  ofile.Write(byte 0);
  ofile.Write(byte 24); // bpp = 24
  ofile.Write(byte 0);

  writeWord 0; // no compression
  writeWord 0; // unspecified image size

  writeWord 8192;   // h. pixels/m
  writeWord 8192;   // v. pixels/m
  writeWord 0; // unspecified colours
  writeWord 0; // all colours are important

  // write a pixel as bytes in order BGR
  let writePixel (r,g,b) =
    let r1 = max 0 (min r 255)
    let g1 = max 0 (min g 255)
    let b1 = max 0 (min b 255)
    ofile.Write(byte b1);
    ofile.Write(byte g1);
    ofile.Write(byte r1)

  // pad to nearest word boundary
  let rec writePad = function
    | 0 -> ()
    | p -> ofile.Write(byte 0); writePad (p-1)

  // write row of pixels
  let rec writeRow row = function
    | 0 -> ()
    | col -> writeRow row (col-1);
             writePixel (cols (col-1,row))

  // write all rows of pixels
  let rec writeRows = function
    | 0 -> ()
    | row -> writeRows (row-1);
             writeRow (row-1) w;
             writePad w3

  writeRows h;

  ofile.Close()


let makeBMParray fname w h (colsArray : (int*int*int) [,]) =
  makeBMP fname w h (fun (i,j) -> colsArray.[i,j])

/// read a file fname.bmp 
let readBMParray fname =
  let ifile = new System.IO.BinaryReader
                   (System.IO.File.Open
                     (fname + ".bmp",
                      System.IO.FileMode.Open,
                      System.IO.FileAccess.Read))

  let checkByte b =
    if b = int (ifile.ReadByte())
    then () else failwith  ("read byte not equal to " + (string b) + "\n")

  let rec skipBytes n =
    if n=0 then ()
    else
      let _ = ifile.ReadByte()
      skipBytes (n-1)

  let readWord () =
    let x0 = int (ifile.ReadByte())
    let x1 = int (ifile.ReadByte())
    let x2 = int (ifile.ReadByte())
    let x3 = int (ifile.ReadByte())
    ((x3*256 + x2)*256 + x1)*256+x0

  let checkWord w =
    let x0 = int (ifile.ReadByte())
    let x1 = int (ifile.ReadByte())
    let x2 = int (ifile.ReadByte())
    let x3 = int (ifile.ReadByte())
    if w = ((x3*256 + x2)*256 + x1)*256+x0
    then () else failwith ("read word not equal to " + (string w) + "\n")

  checkByte 66; // B
  checkByte 77; // M
  let size = readWord()
  checkWord 0;
  let offset = readWord () // offset to data
  let infoSize = readWord () // size of infoheader
//  printfn "offset: %A infosize: %A\n" offset infoSize
  let w = readWord ()  // width
  let h = readWord ()  // height
//  printfn "w: %A h: %A\n" w h
  let w1 = 3*w // width of line of pixels in bytes
  let w3 = (4 - w1%4) % 4
  let w2 = w1 + w3
  checkByte 1;  // planes = 1
  checkByte 0;
  checkByte 24; // bpp = 24
  checkByte 0;

  checkWord 0; // no compression
  let imageSize = readWord (); // perhaps unspecified image size

  let hpm = readWord ()   // h. pixels/m
  let vpm = readWord ()   // v. pixels/m
  let nCols = readWord () // perhaps unspecified colours
  let important = readWord () // are all colours important?
  skipBytes (offset-54)

  let cols = Array2D.create w h (0,0,0)

  // read a pixel as bytes in order BGR
  let readPixel (col,row) =
    let (b,g,r) = (ifile.ReadByte (),ifile.ReadByte (),ifile.ReadByte ())
    cols.[col,row] <- (int r,int g,int b)

  // pad to nearest word boundary
  let rec readPad = function
    | 0 -> ()
    | p -> checkByte 0; readPad (p-1)

  // read row of pixels
  let rec readRow row = function
    | 0 -> ()
    | col -> readRow row (col-1);
             readPixel (col-1,row)

  // red all rows of pixels
  let rec readRows = function
    | 0 -> ()
    | row -> readRows (row-1);
             readRow (row-1) w;
             readPad w3

  readRows h;
  ifile.Close();
  (w,h,cols)

let readBMP fname =
  let (w,h,arr) = readBMParray fname
  (w,h, fun (i,j) -> arr.[i,j])