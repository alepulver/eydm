library(jpeg)
library(png)
library(colorspace)
library(tools)
library(plyr)
library(abind)

channelSpaces = (function() {
  gammaEncode <- function(linear) {
    result <- linear
    lows <- linear <= 0.0031308
    
    result[lows] <- result[lows] * 12.92
    result[!lows] <- (1+0.055) * result[!lows]^(1/2.4) - 0.055
    
    result
  }

  gammaDecode <- function(srgb) {
    result <- srgb
    lows <- srgb <= 0.04045
    
    result[lows] <- result[lows] / 12.92
    result[!lows] <- ((result[!lows]+0.055) / (1+0.055)) ^ 2.4
    
    result
  }

#  gamma_at: function (x) {
#    return Math.log(x)/Math.log(sRGBSpace.encode(x));
#  }

  sRGB <- list(min=0, max=1, fromLinear=gammaDecode, toLinear=gammaEncode)
  linear <- list(min=0, max=1, fromLinear=identity, toLinear=identity)
  
  list(sRGB = sRGB, linear = linear)
})()


colorSpaces = (function() {
  sRGBChannel <- channelSpaces[["sRGB"]]
  cs.sRGB <- list(
    name="sRGB",
    channels=list(red = sRGBChannel, green = sRGBChannel, blue = sRGBChannel),
    fromSRGB = identity, toSRGB = identity
  )
  
  Grey <- list(
    name="Grey",
    channels=list(grey = sRGBChannel),
    fromSRGB = function(imgArr) {
      data <- (imgArr[,,1] + imgArr[,,2] + imgArr[,,3]) / 3
      dim(data) <- c(dim(imgArr)[1:2], 1)
      data
    },
    toSRGB = function(imgArr) {
      data <- abind(imgArr, imgArr, imgArr, along=3)
      dim(data) <- c(dim(imgArr)[1:2], 3)
      data
    }
  )
  
  convertExisting <- function(from, to, csTo) {
    function(imgArr) {
      current <- from(as.vector(imgArr[,,1]), as.vector(imgArr[,,2]), as.vector(imgArr[,,3]))
      data <- array(coords(as(current, to)))
      dim(data) <- dim(imgArr)
      data
    }
  }
  
  cs.HSV.hue <- list(min=0, max=360,
                  fromLinear=channelSpaces$sRGB$fromLinear,
                  toLinear=channelSpaces$sRGB$toLinear)
  cs.HSV <- list(
    name="HSV",
    channels=list(hue = cs.HSV.hue, saturation = sRGBChannel, value = sRGBChannel),
    fromSRGB = convertExisting(RGB, "HSV", "HSV"),
    toSRGB = convertExisting(HSV, "RGB", "sRGB")
  )

  cs.LAB.luminance = list(min=0, max=100, fromLinear=identity, toLinear=identity)
  cs.LAB.ab = list(min=-100, max=100, fromLinear=identity, toLinear=identity)
  cs.LAB <- list(
    name="LAB",
    channels=list(luminance = cs.LAB.luminance, a = cs.LAB.ab, b = cs.LAB.ab),
    fromSRGB = convertExisting(RGB, "LAB", "LAB"),
    toSRGB = convertExisting(LAB, "RGB", "sRGB")
  )
  
  cs.LUV <- list(
    name="LUV",
    channels=list(luminance = cs.LAB.luminance, u = cs.LAB.ab, v = cs.LAB.ab),
    fromSRGB = convertExisting(RGB, "LUV", "LUV"),
    toSRGB = convertExisting(LUV, "RGB", "sRGB")
  )
  
  cs.polarLAB <- list(
    name="polarLAB",
    channels=list(luminance = cs.LAB.luminance, chroma = cs.LAB.ab, hue = cs.LAB.ab),
    fromSRGB = convertExisting(RGB, "polarLAB", "polarLAB"),
    toSRGB = convertExisting(polarLAB, "RGB", "sRGB")
  )
  
  cs.polarLUV <- list(
    name="polarLUV",
    channels=list(luminance = cs.LAB.luminance, chroma = cs.LAB.ab, hue = cs.LAB.ab),
    fromSRGB = convertExisting(RGB, "polarLUV", "polarLUV"),
    toSRGB = convertExisting(polarLUV, "RGB", "sRGB")
  )
  
  result <- list(sRGB = cs.sRGB, Grey = Grey, HSV = cs.HSV,
                 LAB = cs.LAB, LUV = cs.LUV, polarLAB = cs.polarLAB, polarLUV = cs.polarLUV)
  result
})()

my.readImage <- function(path) {
  ext <- tolower(file_ext(path))
  readFunc <- switch(ext,
    jpg=readJPEG,
    jpeg=readJPEG,
    png=readPNG,
    stop(sprintf("Unrecognized file extension: %s\n", ext))
  )
  readFunc(path)
}

imageFromFile <- function(path) {
  img <- my.readImage(path)
  
  if (length(dim(img)) == 2) {
    list(data = img, dim = dim(img)[1:2], colorSpace = colorSpaces$Grey)
  } else if (dim(img)[3] == 3) {
    list(data = img, dim = dim(img)[1:2], colorSpace = colorSpaces$sRGB)
  } else {
    stop("unknown image depth ", dim(img)[3])
  }
}

channelConvCS <- function(ch, channelSpace) {
  if (FALSE && ch$channelSpace == channelSpace) {
    ch
  } else {
    data <- (ch$data - ch$channelSpace$min) / ch$channelSpace$max
    data <- channelSpace$fromLinear(ch$channelSpace$toLinear(data))
    data <- data * (channelSpace$max - channelSpace$min) + channelSpace$min
    dim(data) <- dim(ch$data)
    list(name = ch$name, data = data, dim = ch$dim, channelSpace = channelSpace)
  }
}

imageToChannels <- function(img) {
  llply(1:length(img$colorSpace$channels), function(i) {
    ch <- names(img$colorSpace$channels)[[i]]
    data <- img$data[,,i]
    dim(data) <- img$dim
    list(name = ch, data = data, dim = dim(data),
         channelSpace = img$colorSpace$channels[[i]])
  })
}

imageFromChannels <- function(channels, colorSpace) {
  sizes <- llply(channels, function(ch) { ch$dim })
  #if (length(sizes[[1]]) != 2 || any(sizes != sizes[[1]])) {
  #  stop("channel dimensions must match to compose into an image")
  #}
  
  expectedNames <- laply(channels, function(ch) { ch$name })
  if (any(names(colorSpace$channels) != expectedNames)) {
    stop(sprintf("channel names don't match target color space: %s | %s |",
                 expectedNames, names(colorSpace$channels)))
  }
  
  result <- llply(channels, function(ch) {
    channelConvCS(ch, colorSpace$channels[[ch$name]])
  })
  data <- llply(result, function(ch) { ch$data })
  data <- do.call(abind, list(data, along = 3))
  dim(data) <- c(sizes[[1]], length(channels))
  list(data = data, dim = sizes[[1]], colorSpace = colorSpace)
}

imageMapChannels <- function(transform) {
  
}

imageConvCS <- function(img, colorSpace) {
  if (img$colorSpace$name == colorSpace$name) {
    img
  } else {
    mat <- img$colorSpace$toSRGB(img$data)
    data <- colorSpace$fromSRGB(mat)
    list(data = data, dim = dim(data)[1:2], colorSpace = colorSpace)
  }
}

# FIXME: rewrite

as.raster.SimpleImage <- function(si) {
  img <- si@data
  dim(img) <- si@dim
  img
}

as.raster.CompositeImage <- function(si) {
  img <- si@data
  dim(img) <- si@dim
  img
}