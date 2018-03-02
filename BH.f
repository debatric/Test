      PROGRAM main
      IMPLICIT none
      INTEGER:: n,i,b3,nt,c1,c2
      PARAMETER (nt=60000)
      REAL:: t
      REAL,DIMENSION(:):: s(nt), p(nt), e(nt), m1(nt), m2(nt)
      REAL,DIMENSION(:):: l1(nt), l2(nt), r1(nt), r2(nt)
      REAL,DIMENSION(:):: x1(nt), x2(nt), x3(nt), v1(nt), v2(nt), v3(nt)
      INTEGER,DIMENSION(:):: a1(nt), b1(nt), a2(nt), b2(nt)
      c1=0
      c2=0
      OPEN (UNIT=1,FILE="Binary.dat", STATUS="OLD")
      OPEN (UNIT=2,FILE="primordialbh.txt", STATUS = "NEW")
      OPEN (UNIT=3,FILE="dynamicalbh.txt" , STATUS = "NEW")
      OPEN (UNIT=4,FILE="pbhtime.txt", STATUS = "NEW")
      OPEN (UNIT=5, FILE = "dbhtime.txt", STATUS="NEW")
   14 READ (1,*) n,t
      WRITE(4,*) c1,t
      WRITE(5,*) c2,t
      c1=0
      c2=0
      DO 10 i = 1, 2*n
      READ(1,100) a1(i), a2(i), b1(i), b2(i), b3, e(i), p(i), s(i),
     &          m1(i), m2(i), l1(i), l2(i), r1(i), r2(i),
     &          x1(i), x2(i), x3(i), v1(i), v2(i), v3(i)
      IF (a1(i) < 0) THEN
      GOTO 14
      ENDIF
      IF ((b1(i)==14).AND.(b2(i)==14))THEN
      GOTO 24
   24 IF ((a1(i)==a2(i)+1).OR.(a1(i)==a2(i)-1))THEN
      WRITE(2,200) a1(i), a2(i), b1(i), b2(i), e(i), p(i), s(i), m1(i),
     &m2(i), l1(i), l2(i), r1(i), r2(i), x1(i), x2(i), x3(i), v1(i),
     &v2(i), v3(i), t
      c1=c1+1
      ELSE
      WRITE(3,200) a1(i), a2(i), b1(i), b2(i), e(i), p(i), s(i), m1(i),
     &m2(i), l1(i), l2(i), r1(i), r2(i), x1(i), x2(i), x3(i), v1(i),
     &v2(i), v3(i), t
      c2=c2+1
      ENDIF
      ENDIF
   10 CONTINUE

 100  FORMAT(2I7,2I3,I4, F6.3,F10.2,7F8.3,1P,6E14.6,0P,2F12.3,1P,2E12.4)
 200  FORMAT(2I7,2I3, F6.3,
     &F10.2,7F8.3,1P,6E14.6,0P,2F12.3,1P,2E12.4,F18.8)
      RETURN
      END

