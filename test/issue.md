known issue

# 関数定義で定義していない関数を参照しても型検査を通る
```
type qubit = unit + unit
func fdistrib : ((unit + qubit) * octet) <-> (unit * octet) + (qubit * octet) = distrib
```

# 定義した型と同じ型を同一の型として扱えない
```
type qubit = unit + unit
func test : qubit <-> unit + unit = id
```
