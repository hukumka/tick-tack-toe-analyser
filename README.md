# tick-tack-toe game analyser

https://en.wikipedia.org/wiki/Tic-tac-toe

For each possible state of game (3x3) computes:
  + Existance of win strategy
  + Invetablility of loss
using minmax algorithm

https://en.wikipedia.org/wiki/Minimax

result stored in file "output.txt"

## result format

Result represented with text file consisting of several records
of following format

```
line N+0: State{
line N+1:   a11a12a13
line N+2:   a21a22a23
line N+3:   a31a32a33
line N+4: } = Analysis{player: p, will_win: w, will_lose: l}
```
###### Note: "line N+i:" does not actualy appear in file

where:
  + a11..a33 - single character of set
    + X - current cell contains cross
    + O - current cell containt zero
    + _ - current cell is empty
  + p - shows who will make next move (Cross or Zero)
  + w - shows if winning strategy for player p exists
  + l - shows if winning strategy for p opponent exists
