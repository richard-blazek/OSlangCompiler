# O'Slang compiler
This is the source code of my compiler of my O'Slang language.

## Operator precedence
0. fun (= function literal), if (= conditional), while (= loop), ( ) (= parentheses)
1. . (= access), [ ] (= call), $ (= dereference); postfix
2. ~ (= unary minus), ! (= negation); prefix
3. *, /, %, >>, << (= bitshifts); left associative infix
4. +, -, &, |, ^ (bitwise or logical); left associative infix
5. ==, !=, <, >, >=, <= (comparison); non-associative infix
6. <- (= assignment); right-associative infix

```
type Point[x, y: i32]

main == fun[]
    p <- Point[7, 4]
    p.x <- p.x$ + 1
    p.y <- p$.y + 1

    a <- b <- 0
    a <- a$ + 1

    c <- log[5+5]
end
```
