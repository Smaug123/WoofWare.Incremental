# WoofWare.Incremental

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.Incremental.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.Incremental)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.Incremental/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.Incremental/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.Incremental)](./LICENCE.md)

This is [`incremental`](https://github.com/janestreet/incremental), but in F#.

To quote the original documentation:

Incremental is a library that gives you a way of building complex computations that can update efficiently in response to their inputs changing, inspired by the work of [[http://www.umut-acar.org/self-adjusting-computation][Umut Acar et. al.]] on self-adjusting computations.
Incremental can be useful in a number of applications, including:

- Building large calculations (of the kind you might build into a spreadsheet) that can react efficiently to changing data.
- Constructing views in GUI applications that can incorporate new data efficiently.
- Computing derived data while guaranteeing that the derived data stays in sync with the source data, for instance filtering or inversing a mapping.

You can find detailed documentation of the library and how to use it in [[https://github.com/janestreet/incremental/blob/master/src/incremental_intf.ml][incremental/src/incremental_intf.ml]].
You can also find an informal introduction to the library in this [[https://blog.janestreet.com/introducing-incremental][blog post]] and [this video](https://www.youtube.com/watch?v=G6a5G5i4gQU).

# Licence

This is a derivative work of [incremental](https://github.com/janestreet/incremental/tree/4c3946aafe786e4846f8ec3f4825e7bc689a70fa), used under the MIT licence.
A copy of that licence is at [LICENCE_janestreet.md](LICENCE_janestreet.md).
All glory to Jane Street.

WoofWare.Incremental is licenced to you under the MIT licence.
A copy of that licence is at [LICENCE.md](LICENCE.md).
