### In development

```shell
pulp -w build --to bin/fluid.js
```

### Build

Have `pkg` installed.

```shell
npm i pkg
```


```shell
pulp build --to bin/fluid.js
cd bin
pkg fluid.js
cd ..
```

`pkg` will create `bin/fluid-linux`, `bin/fluid-osx`, and `bin/fluid-win.exe`.

