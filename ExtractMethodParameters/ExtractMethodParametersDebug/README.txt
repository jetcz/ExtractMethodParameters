This project is in the solution ONLY to be able to debug this extension in experimental instance of Visual Studio.

To be able to debug VSIX project in experimental instance of Visual Studio, the VSIX project needs to be targeted to .NET Framework 4.8.
However the .vsix file cannot be installed to Visual Studio 2022 :-)

To be able to install the .vsix to Visual Studio, the VSIX project needs to be targeted to .NET 6.0 or newer.
However that project cannot be debugged in VS as it won't load any symbols :-)

Fuck you Microsoft :-)