# omnirev

Typed reversible programming language based on linear type and it's compiler.

The reversibility is guaranteed by type inference and exhaustiveness check.



# Setup

## install stack

```bash
sudo apt install -y stack
```

## build omnirev

1. clone this project
```bash
git clone https://github.com/MountainSeal/omnirev
```
2. change directory
```bash
cd omnirev
```

3. build this project and install
```bash
stack install
```

# Usage

see help
```bash
omnirev --help
```

# Licence

see LICENSE.

# ToDo (Implementation) List

- [x] type inference
- [x] exhaustiveness check
  - Not sure if it works properly
- [x] interpreter
  - MANY BUGS
- [x] [online playground](https://omnirev.dev/)
  - [x] syntax highlighter
  - [x] web server
- [ ] REPL
