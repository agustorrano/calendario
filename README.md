# TP Final - ALP - 2024

## Introducción
Este README es un documento complementario al informe del TP.

### Cabal
Para correr el calendario basta con ejecutar:
```
cabal run
```
Las operaciones soportadas por el calendario pueden verse utilizando el comando `:help` o `:?`:
```
Cal> :help
Lista de comandos:

:load <file>, :l <file>       Cargar un programa desde un archivo
:reload, :r                   Volver a cargar el último archivo
:print, :p                    Imprime el calendario actual
:quit, :q                     Salir del intérprete
:help, :?                     Mostrar esta lista de comandos
:ops, :o                      Mostrar las operaciones del calendario
:close, :c                    Cerrar y guardar el calendario actual
:export, :e                   Exportar un calendario a un archivo .ics
:import <file>, :i <file>     Importar un calendario .ics
```

### Estructura del código
La estructura del proyecto es la siguiente:
```
.
├── app
│   └── Main.hs
├── src
│   ├── CalendarOps.hs
│   ├── Common.hs
│   ├── ICS.hs
│   ├── Parse.hs
│   ├── PrettyPrinter.hs
│   └── RecurrenceOps.hs
├── Ejemplos
│   ├── Clases.ics
│   ├── ejemplo1.cal
│   ├── ejemplo2.cal
├── calendario.cabal
├── README.md
├── Setup.hs
└── Informe.pdf
```
* En el directorio `app` se define el módulo `Main`, que implementa el ejecutable final. 

* En el directorio `src` se encuentran los módulos:
  - `CalendarOps` tiene las implementaciones de todas las operaciones del calndario.
  - `Common` define los tipos de datos y comandos. 
  - `ICS` tiene las operaciones utilizadas para la importación y exportación de archivos .ics.
  - `Parser` tiene el esqueleto para el parser.
  - `PrettyPrinter` tiene el Pretty Printer del lenguaje. Este sirve para imprimir los programas de una manera más legible que haciendo `show`.
  - `RecurrenceOps` tiene las operaciones que manejan la recurrencia de eventos.

* En el directorio `Ejemplos` hay algunos -*shock*- ejemplos de archivos .cal y .ics. 

* El resto de los archivos son de configuración del proyecto y el informe.

### Entorno interactivo con GHCi
También se puede cargar un módulo específico del proyecto en el entorno interactivo GHCi:
```
cabal exec ghci -- -isrc src/Parse.hs
```
La bandera `-isrc` es necesaria para indicarle a GHCi que los archivos que importa el módulo que estamos cargando deben ser buscados dentro de la carpeta `src/`.

Alternativamente, se puede inicializar GHCi:
```
cabal exec ghci
```
Y luego cargar el archivo deseado desde allí:
```
ghci> :cd src/
ghci> :load Parse.hs
[1 of 2] Compiling Common           ( Common.hs, interpreted )
[2 of 2] Compiling Parse            ( Parse.hs, interpreted )
Ok, two modules loaded.
```
