C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,1)
C     
      SUBROUTINE FFV1L2_1(P2, V3, COUP, M1, W1, P1, COEFF)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      REAL*8 W1
      COMPLEX*16 V3(*)
      COMPLEX*16 COUP
      INCLUDE 'coef_specs.inc'
      COMPLEX*16 COEFF(MAXLWFSIZE,0:VERTEXMAXCOEFS-1,MAXLWFSIZE)
      COMPLEX*16 P1(0:3)
      REAL*8 M1
      COMPLEX*16 P2(0:3)
      P1(0) = +P2(0)+V3(1)
      P1(1) = +P2(1)+V3(2)
      P1(2) = +P2(2)+V3(3)
      P1(3) = +P2(3)+V3(4)
      COEFF(1,0,1)= COUP*(-1D0)*(P1(0)*(-CI*(V3(5))+CI*(V3(8)))+(P1(1)
     $ *(+CI*(V3(6))+V3(7))+(P1(2)*(-V3(6)+CI*(V3(7)))+P1(3)*(-CI
     $ *(V3(5))+CI*(V3(8))))))
      COEFF(2,0,1)= COUP*(P1(0)*(-1D0)*(+CI*(V3(6))+V3(7))+(P1(1)*(+CI
     $ *(V3(5))-CI*(V3(8)))+(P1(2)*(V3(5)-V3(8))+P1(3)*(+CI*(V3(6))
     $ +V3(7)))))
      COEFF(3,0,1)= COUP*M1*(+CI*(V3(5))-CI*(V3(8)))
      COEFF(4,0,1)= COUP*-M1*(+CI*(V3(6))+V3(7))
      COEFF(1,1,1)= COUP*(+CI*(V3(5))-CI*(V3(8)))
      COEFF(2,1,1)= COUP*(-1D0)*(+CI*(V3(6))+V3(7))
      COEFF(3,1,1)= 0D0
      COEFF(4,1,1)= 0D0
      COEFF(1,2,1)= COUP*(-1D0)*(+CI*(V3(6))+V3(7))
      COEFF(2,2,1)= COUP*(+CI*(V3(5))-CI*(V3(8)))
      COEFF(3,2,1)= 0D0
      COEFF(4,2,1)= 0D0
      COEFF(1,3,1)= COUP*(-1D0)*(-V3(6)+CI*(V3(7)))
      COEFF(2,3,1)= COUP*(V3(5)-V3(8))
      COEFF(3,3,1)= 0D0
      COEFF(4,3,1)= 0D0
      COEFF(1,4,1)= COUP*(+CI*(V3(5))-CI*(V3(8)))
      COEFF(2,4,1)= COUP*(+CI*(V3(6))+V3(7))
      COEFF(3,4,1)= 0D0
      COEFF(4,4,1)= 0D0
      COEFF(1,0,2)= COUP*(P1(0)*(-CI*(V3(6))+V3(7))+(P1(1)*(+CI*(V3(5)
     $ +V3(8)))+(P1(2)*(-1D0)*(V3(5)+V3(8))+P1(3)*(-CI*(V3(6))+V3(7))))
     $ )
      COEFF(2,0,2)= COUP*(-1D0)*(P1(0)*(-1D0)*(+CI*(V3(5)+V3(8)))
     $ +(P1(1)*(+CI*(V3(6))-V3(7))+(P1(2)*(V3(6)+CI*(V3(7)))+P1(3)*(
     $ +CI*(V3(5)+V3(8))))))
      COEFF(3,0,2)= COUP*-M1*(+CI*(V3(6))-V3(7))
      COEFF(4,0,2)= COUP*M1*(+CI*(V3(5)+V3(8)))
      COEFF(1,1,2)= COUP*(-1D0)*(+CI*(V3(6))-V3(7))
      COEFF(2,1,2)= COUP*(+CI*(V3(5)+V3(8)))
      COEFF(3,1,2)= 0D0
      COEFF(4,1,2)= 0D0
      COEFF(1,2,2)= COUP*(+CI*(V3(5)+V3(8)))
      COEFF(2,2,2)= COUP*(-1D0)*(+CI*(V3(6))-V3(7))
      COEFF(3,2,2)= 0D0
      COEFF(4,2,2)= 0D0
      COEFF(1,3,2)= COUP*(-V3(5)-V3(8))
      COEFF(2,3,2)= COUP*(-1D0)*(V3(6)+CI*(V3(7)))
      COEFF(3,3,2)= 0D0
      COEFF(4,3,2)= 0D0
      COEFF(1,4,2)= COUP*(-1D0)*(+CI*(V3(6))-V3(7))
      COEFF(2,4,2)= COUP*(-1D0)*(+CI*(V3(5)+V3(8)))
      COEFF(3,4,2)= 0D0
      COEFF(4,4,2)= 0D0
      COEFF(1,0,3)= COUP*M1*(+CI*(V3(5)+V3(8)))
      COEFF(2,0,3)= COUP*M1*(+CI*(V3(6))+V3(7))
      COEFF(3,0,3)= COUP*(-1D0)*(P1(0)*(-1D0)*(+CI*(V3(5)+V3(8)))
     $ +(P1(1)*(+CI*(V3(6))+V3(7))+(P1(2)*(-V3(6)+CI*(V3(7)))+P1(3)*(
     $ +CI*(V3(5)+V3(8))))))
      COEFF(4,0,3)= COUP*(P1(0)*(+CI*(V3(6))+V3(7))+(P1(1)*(-1D0)*(+CI
     $ *(V3(5)+V3(8)))+(P1(2)*(-1D0)*(V3(5)+V3(8))+P1(3)*(+CI*(V3(6))
     $ +V3(7)))))
      COEFF(1,1,3)= 0D0
      COEFF(2,1,3)= 0D0
      COEFF(3,1,3)= COUP*(+CI*(V3(5)+V3(8)))
      COEFF(4,1,3)= COUP*(+CI*(V3(6))+V3(7))
      COEFF(1,2,3)= 0D0
      COEFF(2,2,3)= 0D0
      COEFF(3,2,3)= COUP*(-1D0)*(+CI*(V3(6))+V3(7))
      COEFF(4,2,3)= COUP*(-1D0)*(+CI*(V3(5)+V3(8)))
      COEFF(1,3,3)= 0D0
      COEFF(2,3,3)= 0D0
      COEFF(3,3,3)= COUP*(-1D0)*(-V3(6)+CI*(V3(7)))
      COEFF(4,3,3)= COUP*(-V3(5)-V3(8))
      COEFF(1,4,3)= 0D0
      COEFF(2,4,3)= 0D0
      COEFF(3,4,3)= COUP*(-1D0)*(+CI*(V3(5)+V3(8)))
      COEFF(4,4,3)= COUP*(+CI*(V3(6))+V3(7))
      COEFF(1,0,4)= COUP*M1*(+CI*(V3(6))-V3(7))
      COEFF(2,0,4)= COUP*M1*(+CI*(V3(5))-CI*(V3(8)))
      COEFF(3,0,4)= COUP*(P1(0)*(+CI*(V3(6))-V3(7))+(P1(1)*(-CI*(V3(5))
     $ +CI*(V3(8)))+(P1(2)*(V3(5)-V3(8))+P1(3)*(-CI*(V3(6))+V3(7)))))
      COEFF(4,0,4)= COUP*(-1D0)*(P1(0)*(-CI*(V3(5))+CI*(V3(8)))+(P1(1)
     $ *(+CI*(V3(6))-V3(7))+(P1(2)*(V3(6)+CI*(V3(7)))+P1(3)*(-CI*(V3(5)
     $ )+CI*(V3(8))))))
      COEFF(1,1,4)= 0D0
      COEFF(2,1,4)= 0D0
      COEFF(3,1,4)= COUP*(+CI*(V3(6))-V3(7))
      COEFF(4,1,4)= COUP*(+CI*(V3(5))-CI*(V3(8)))
      COEFF(1,2,4)= 0D0
      COEFF(2,2,4)= 0D0
      COEFF(3,2,4)= COUP*(-CI*(V3(5))+CI*(V3(8)))
      COEFF(4,2,4)= COUP*(-1D0)*(+CI*(V3(6))-V3(7))
      COEFF(1,3,4)= 0D0
      COEFF(2,3,4)= 0D0
      COEFF(3,3,4)= COUP*(V3(5)-V3(8))
      COEFF(4,3,4)= COUP*(-1D0)*(V3(6)+CI*(V3(7)))
      COEFF(1,4,4)= 0D0
      COEFF(2,4,4)= 0D0
      COEFF(3,4,4)= COUP*(-1D0)*(+CI*(V3(6))-V3(7))
      COEFF(4,4,4)= COUP*(+CI*(V3(5))-CI*(V3(8)))
      END


