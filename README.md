# haskell-dot-gif

[![Hackage](https://img.shields.io/hackage/v/haskell-dot-gif.svg?logo=haskell)](https://hackage.haskell.org/package/haskell-dot-gif)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Este repositorio demuestra algunas técnicas para la animación usando Haskell y FRP.

## descripción
En el nivel base del repositorio hay unos gifs. Cada uno de estos se corresponde
a uno de los programas realizados en `src/HaskellDotGif.hs`.

## compilar
para reproducir alguno de estos gifs y experimentar con los parámetros, siga los
siguientes pasos.

### con nix
- instala nix:

``` sh
curl -L https://nixos.org/nix/install | sh
```

- clona el respositorio y activa el entorno de desarrollo.

``` sh
git clone https://github.com/LuisChDev/haskell-dot-gif && cd haskell-dot-gif
nix-shell
```

- una vez terminada la carga tendrás una shell con todos los elementos para
  desarrollar en haskell con Yampa y diagrams. Puedes usar GHCi para cargar e 
  interactuar con el código:
  
``` sh
cd src && ghci
Prelude> :l HaskellDotGif
Ok, 1 module loaded.
Prelude HaskellDotGif> :set args -o rainbow2.gif -w 400 -h 400
Prelude HaskellDotGif> animate
animate
```
