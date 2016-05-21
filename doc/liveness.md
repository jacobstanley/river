# Imperative

```
   +---+--------------------+-----------+--------+-----------+
   | # | Instructions       | Pass 1    | Pass 2 | All       |
   +---+--------------------+-----------+--------+-----------+
 ^ | 1 | if (x2 = 0) goto 8 | x1, x2    |        | x1, x2    |
 | | 2 | q <- x1 / x2       | x1, x2    |        | x1, x2    |
 | | 3 | t <- q * x2        | x1, x2, q |        | x1, x2, q |
 | | 4 | r <- x1 - t        | x1, x2, t |        | x1, x2, t |
 | | 5 | x1 <- x2           | x2, r     |        | x2, r     |
 | | 6 | x2 <- r            | r         | x1     | x1, r     |
 | | 7 | goto 1             | .         | x1, x2 | x1, x2    |
 . | 8 | return x1          | x1        |        | x1        |
   +---+--------------------+-----------+--------+-----------+
```

# Functional

```
gcd (x1, x2) =
  if (x2 = 0)         -- x1, x2
    return x1         -- x1
  else
    let
      q   = x1 / x2   -- x1, x2
      t   = q * x2    -- x1, x2, q
      r   = x1 - t    -- x1, x2, t
      x1' = x2        -- x2, r
      x2' = r         -- x1', r
    in
      gcd (x1', x2')  -- x1', x2'
```

```
gcd (x1, x2) =
  if (x2 = 0)         -- x1, x2
    return x1         -- x1
  else
    let
      q = x1 / x2     -- x1, x2
      t = q * x2      -- x1, x2, q
      r = x1 - t      -- x1, x2, t
    in
      gcd (x2, r)     -- x2, r
      =>
      x1 <- x2        -- x2, r
      x2 <- r         -- x1, r
      jump gcd.       -- x1, x2
```

# SSA

```
x1_a <- ??
x2_a <- ??              -- x1_a
goto gcd                -- x1_a, x2_a

gcd:
  x1 <- phi(x1_a, x1_b) -- x1_a, x1_b, x2_a, x2_b
  x2 <- phi(x2_a, x2_b) -- x1, x2_a, x2_b
  if (x2 = 0)           -- x1, x2
    return x1           -- x1
  else
    q    <- x1 / x2     -- x1, x2
    t    <- q * x2      -- x1, x2, q
    r    <- x1 - t      -- x1, x2, t
    x1_b <- x2          -- x2, r
    x2_b <- r           -- x1_b, r
    goto gcd            -- x1_b, x2_b
```
