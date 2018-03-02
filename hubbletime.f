





      END FUNCTION func

      SUBROUTINE trapzd(func,a,b,s,n)
      IMPLICIT NONE
      INTEGER :: n
      REAL*8,INTENT(IN):: a,b
      EXTERNAL func
      INTEGER:: it,j
      REAL*8 :: del,sum,tnm,x,s,func
*      parameter (n=10000)
      it=2**(n-2)
      it = 50000
      tnm=float(it)
      del=(b-a)/tnm
      x=a+0.5*del
      sum=0.5*func(a)
*      write(*,*)' trap ',it,tnm,del,x
      do 11 j=1,it-1
      sum=sum+func(x)
      x=x+del
*     write(*,*)j,x,sum
  11  continue
      sum=sum+0.5*func(b)
      s=sum*del
      return

      END subroutine




      program main
      IMPLICIT NONE
      REAL*8 ::a,b,a0,e0,c0,m1,m2,p,r,q,l3,l1,sf,s,func
      INTEGER :: n
      EXTERNAL func
      WRITE (*,*) "a0= ,e0=, m1= ,m2= "
      READ(*,*) a0,e0,m1,m2
*      a0 = 5.
*      e0 = 0.5
*      m1 = 10.
*      m2 = 10.
*      a0=a0*7.0e+08
      p=-12./19.
      r=121./304.
      q=-870./2299.
      n=10
      c0=a0*(e0**p)*((1+r*(e0**2))**q)
      l3=1.9e+16
      l1=((m1*m2)/m1+m2)
      a=0.
      b=e0
      write(*,*) a,b,c0,l1,l3
      call trapzd(func,a,b,s,n)
      write(*,*) "trapezoidal : " , s
      sf=(12./19.)*(c0**4./l1)*s*l3
      write(*,*) "The time is =", sf/3.16e+13
      stop
      end

