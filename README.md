# SCANWEB

Web app interface for sane "scanimage". Enabling scanning on printserver remotely from any web browser.
With Erlang [cowboy](https://github.com/ninenines/cowboy) and [Erlang.mk](https://erlang.mk/guide/index.html) as the build system.


## Build

Clone 
```
git clone https://github.com/tehsmeely/ScanWeb.git
```
and run make to install deps and build release
```
cd ScanWeb
make
```

run release from release script
```
_rel/scanweb_release/bin/scanweb_release start
```
