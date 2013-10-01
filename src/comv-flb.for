C+*********************************************************** comv-flb.f
C@NBI PGS 2000Jul20 - This subroutine not being used any more, so commented out
CCCh*********************************************************************
CC      SUBROUTINE MATMULT(mp, np, rp, m, n, r, a, b, c)
CCC
CCC     Routine for Matrix multiplication  a * b = c
CCC
CCC     H. Rickert, adapted by V. Dorer  24.Nov 1994
CCC
CCC     Input:  mp, np , rp integer    Max dimension of Matrix a (mp, np)
CCC                                    Max dimension of Matrix b (np, rp)
CCC                                    Max dimension of Matrix c (mp, rp)
CCC
CCC             m, n, r     integer    Actual dimensions of Matrix a,b,c
CCC
CCC             a           REAL       Matrix a
CCC             b           REAL       Matrix b
CCC
CCC     Output: c           REAL       Matrix c
CCCh*********************************************************************
CC
CC        IMPLICIT NONE
CC      INTEGER mp, np,rp, m, n, r
CC      REAL a(mp, np), b(np, rp ), c(mp, rp)
CC
CC      INTEGER i, l, k
CC
CC      do 100 i = 1, m
CC          do 100 k = 1, r
CC              c(i,k) = 0.
CC              do 100 l = 1, n
CC                  c(i,k) = c(i,k) + ( a(i,l) * b(l,k) )
CC100   continue
CC      end


C@NBI PGS 2000Jul20 - This subroutine not being used any more, so commented out
CCCh*********************************************************************
CC      SUBROUTINE MATVECMULT(mp, np, m, n, a, b, c)
CC
CCC
CCC     Routine for multiplication of matrix with vector a * b = c
CCC
CCC     H. Rickert, adapted by V. Dorer  24.Nov 1994
CCC
CCC     Input:  mp, np      integer    Max dimension of Matrix a (mp, np)
CCC                                    Max dimension of vector b (np)
CCC                                    Max dimension of Matrix c (mp)
CCC
CCC             m, n        integer    Actual dimensions of Matrix a,b,c
CCC
CCC             a           REAL       Matrix a
CCC             b           REAL       Vector b
CCC
CCC     Output: c           REAL       Vector c
CCCh*********************************************************************
CC
CC        IMPLICIT NONE
CC      INTEGER mp, np, m, n
CC      REAL a(mp, np), b(np), c(mp)
CC
CC      INTEGER i, l
CC
CC      do 100 i = 1, m
CC          c(i) = 0.
CC          do 100 l = 1, n
CC              c(i) = c(i) + ( a(i,l) * b(l) )
CC100   continue
CC      end


Ch**********************************************************************
      SUBROUTINE INVMAT (A, Y, n, np, index, vv,errkey)
C
C     Calculates the inverse of A matrix a and writes the result on Y
C
C     NOTE: Matrix A will be overwritten and cannot be used afterwards
C
C     H. Rickert/V. Dorer 24. Nov 1994
C
C     Based on 'Numerical recipes (FORTRAN)' , page 38
C     Cambridge University Press, 1989
C
C     Uses  LUDCMP and LUBKSB
C
C     IO  nr   Name      Type    Description
C     ---------------------------------------------------------------
C     I(O)1    A (np,np)  REAL    Input matrix a
C                                 NOTE: Matrix a will be overwritten!!
C     O   2    Y (np,np)  REAL    Inverted matrix
C     I   3    n          int     Actual dimension of matrix a
C     I   4    np         int     Max dimension of a, index, and vv
C     -   5    index(np)  int   } Used only internally in LUDCMP and LUBKSB
C                               } Passed because of dimensioning purposes
C     -   6    vv (np)    REAL  } Must be dimensioned with np in the
C                                 calling routine
C
Ch**********************************************************************

        IMPLICIT NONE
      INTEGER  i,j,n, np, index(np)
      logical errkey
      REAL a(np,np) , y(np,np) , vv(np)

      errkey=.false.
      do 12 i = 1, n
          do 11 j = 1, n
              y(i,j) = 0.
11        continue
          y(i,i) = 1.
12    continue
      call LUDCMP(a, n, np, index, vv,errkey)
      if (errkey)  return
      do 13 j = 1, n
          call LUBKSB(a, n, np, index, y(1,j))
13    continue

      end


Ch**********************************************************************
      SUBROUTINE LUDCMP(a,n,np,indx,vv,errkey)
C
C     LU Decomposition of matrix a
C
C     Based on 'Numerical recipes (FORTRAN)' , page 35
C     Cambridge University Press, 1989

C     To be used in combination with routine LUBKSB  to solve
C     linear equations or to invert a matrix
C
C  (C) Copr. 1986-92 Numerical Recipes Software n#w1`-0!$5%A_12&.
C
C     IO  nr   Name    Unit       Description
C     ---------------------------------------------------------------
C     IO  1    a       REAL       Matrix input/ output inverted matrix
C     I   2    n       integer    Actual dimension of matrix a
C     I   3    np      integer    Max dimension of matrix a
C     O   4    indx    integer    Vector for row permutation
C     O   5    vv      REAL       used only internally, passed onyl because
C                                 of dimensioning
C
Ch*********************************************************************

        IMPLICIT NONE
      INTEGER n,np,indx(n)
      REAL d,a(np,np),vv(np),TINY
      PARAMETER (TINY=1.0e-20)
      INTEGER i,imax,j,k
      REAL aamax,dum,sum
      LOGICAL errkey

      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) then
          errkey=.true.
          return
        endif
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=TINY
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      END


Ch**********************************************************************
      SUBROUTINE LUBKSB(a,n,np,indx,b)
C
C     Routine for forward and backward substition
C     to be used in combination with routine LUDCMP
C
C     Based on 'Numerical recipes (FORTRAN)' , page 36
C     Cambridge University Press, 1989
C
C  (C) Copr. 1986-92 Numerical Recipes Software n#w1`-0!$5%A_12&.
C
C     IO  nr   Name    Unit       Description
C     ---------------------------------------------------------------
C     IO  1    a       REAL       Decomposed matrix (output from LUDCMP)
C     I   2    n       integer    Actual dimension of matrix a
C     I   3    np      integer    Max dimension of matrix a
C     I   4    indx    integer    Permuation vector (output from LUDCMP)
C     IO   5   b       integer    Input: Right hand side of linear
C                                 equation a * x = b
C                                 Output: Solution vector x
C
Ch*********************************************************************
        IMPLICIT NONE
      INTEGER n,np,indx(n)
      REAL a(np,np),b(n)
      INTEGER i,ii,j,ll
      REAL sum
      ii=0

      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      END


