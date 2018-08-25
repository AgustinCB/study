Sure.

```
import Data.List.intercalate

expressions2RPN :: [Expression] -> String
expressions2RPN expressions =
  intercalate " " (map expression2RPN expressions)

expression2RPN :: Expression -> String
expression2RPM (Binary right operator left) = (expression2RPM right) ++ " " ++ (expression2RPM left) ++ " " ++ (show operator)
expression2RPM (Unary operator operand) = (expression2RPM right) ++ " " ++ (show operand)
expression2RPM (Grouping expression) = "(" ++ (expression2RPM expression) ++ ")"
expression2RPM (ExpressionLiteral value) = show value
```
