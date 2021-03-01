Stock Price Prediction Project
=============

![CI Status](https://github.com/caiormt/stock-price-prediction/workflows/ci/badge.svg)
[![codecov](https://codecov.io/gh/caiormt/stock-price-prediction/branch/main/graph/badge.svg?token=MP7ATEUP57)](https://codecov.io/gh/caiormt/stock-price-prediction)

## Description

This project aims to predict future stock values using Sequence Alignment, translating historic values from markets into a known alphabet.

Current markets implemented:
- [Ibovespa][b3-history]

## Setup

First install the following packages with your favorite dependency manager:

* JDK 8
* [Mill Build Tool][mill]

## Importing with text editor ou IDE

### Support for VSCode + Metals

[Metals with VSCode][metals-vscode] automatically detects the Mill Build.  
Just run: `Metals: Import build` on Visual Studio Code.

### Support for IntelliJ IDEA

Install [Scala Plugin][intellij-scala] on IntelliJ.

Export dependency files to IntelliJ Structure:
```shell script
$ mill mill.scalalib.GenIdea/idea
``` 

And import the project as usual.

### Check dependecy upgrade

```shell script
$ mill mill.scalalib.Dependency/showUpdates
```

## LICENSE

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.
If not, see <https://www.gnu.org/licenses/>.

[mill]: http://www.lihaoyi.com/mill/#installation
[metals-vscode]: https://scalameta.org/metals/docs/editors/vscode.html
[intellij-scala]: https://plugins.jetbrains.com/plugin/1347-scala
[b3-history]: http://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/historico/mercado-a-vista/cotacoes-historicas/
