PROGRAM QUAD
 IMPLICIT NONE
 REAL :: a,b,c,r1,r2,d
 PRINT *, "Input values of a,b and c"
 READ *,a,b,c
 IF (a == 0.0) THEN
        IF (b == 0.0) THEN
            IF (c == 0.0) THEN
                PRINT *, 'The equation has infinitely many solutions.'
            ELSE
                PRINT *, 'No solution: The equation is inconsistent.'
            END IF
        ELSE
            r1 = -c / b
            PRINT *, 'This is a linear equation. The root is: ', r1
        END IF
 ELSE
 d=(b*b)-4.0*a*c
 IF (d .GT. 0.0) THEN
        r1 = (-b + sqrt(d)) / (2.0 * a)
        r2 = (-b - sqrt(d)) / (2.0 * a)
        PRINT *, "The real roots are: ", r1, r2
 ELSE IF (d.LT.0)THEN
  PRINT *, "Roots are imaginary"
 END IF
 IF (d.EQ.0)THEN
  PRINT *, "There is one real root"
  r1=-b/(2*a)
  PRINT *, "r = ",r1
 END IF
 END IF
END PROGRAM QUAD

