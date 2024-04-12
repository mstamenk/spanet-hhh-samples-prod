      SUBROUTINE ML5_0_0_1_MP_COEF_CONSTRUCTION_1(P,NHEL,H,IC)
C     
      USE ML5_0_0_1_POLYNOMIAL_CONSTANTS
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=5)
      INTEGER    NCOMB
      PARAMETER (NCOMB=4)

      INTEGER    NLOOPS, NLOOPGROUPS, NCTAMPS
      PARAMETER (NLOOPS=100, NLOOPGROUPS=100, NCTAMPS=14)
      INTEGER    NLOOPAMPS
      PARAMETER (NLOOPAMPS=114)
      INTEGER    NWAVEFUNCS,NLOOPWAVEFUNCS
      PARAMETER (NWAVEFUNCS=12,NLOOPWAVEFUNCS=216)
      REAL*16     ZERO
      PARAMETER (ZERO=0.0E0_16)
      COMPLEX*32     IZERO
      PARAMETER (IZERO=CMPLX(0.0E0_16,0.0E0_16,KIND=16))
C     These are constants related to the split orders
      INTEGER    NSO, NSQUAREDSO, NAMPSO
      PARAMETER (NSO=1, NSQUAREDSO=1, NAMPSO=1)
C     
C     ARGUMENTS
C     
      REAL*16 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
      INTEGER H
C     
C     LOCAL VARIABLES
C     
      INTEGER I,J,K
      COMPLEX*32 COEFS(MAXLWFSIZE,0:VERTEXMAXCOEFS-1,MAXLWFSIZE)
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'mp_coupl_same_name.inc'

      INTEGER GOODHEL(NCOMB)
      LOGICAL GOODAMP(NSQUAREDSO,NLOOPGROUPS)
      COMMON/ML5_0_0_1_FILTERS/GOODAMP,GOODHEL

      INTEGER SQSO_TARGET
      COMMON/ML5_0_0_1_SOCHOICE/SQSO_TARGET

      LOGICAL UVCT_REQ_SO_DONE,MP_UVCT_REQ_SO_DONE,CT_REQ_SO_DONE
     $ ,MP_CT_REQ_SO_DONE,LOOP_REQ_SO_DONE,MP_LOOP_REQ_SO_DONE
     $ ,CTCALL_REQ_SO_DONE,FILTER_SO
      COMMON/ML5_0_0_1_SO_REQS/UVCT_REQ_SO_DONE,MP_UVCT_REQ_SO_DONE
     $ ,CT_REQ_SO_DONE,MP_CT_REQ_SO_DONE,LOOP_REQ_SO_DONE
     $ ,MP_LOOP_REQ_SO_DONE,CTCALL_REQ_SO_DONE,FILTER_SO

      COMPLEX*32 W(20,NWAVEFUNCS)
      COMMON/ML5_0_0_1_MP_W/W

      COMPLEX*32 WL(MAXLWFSIZE,0:LOOPMAXCOEFS-1,MAXLWFSIZE,
     $ -1:NLOOPWAVEFUNCS)
      COMPLEX*32 PL(0:3,-1:NLOOPWAVEFUNCS)
      COMMON/ML5_0_0_1_MP_WL/WL,PL

      COMPLEX*32 AMPL(3,NLOOPAMPS)
      COMMON/ML5_0_0_1_MP_AMPL/AMPL

C     
C     ----------
C     BEGIN CODE
C     ----------

C     The target squared split order contribution is already reached
C      if true.
      IF (FILTER_SO.AND.MP_LOOP_REQ_SO_DONE) THEN
        GOTO 1001
      ENDIF

C     Coefficient construction for loop diagram with ID 1
      CALL MP_FFV1L1_2(PL(0,0),W(1,1),GC_5,MDL_MB,ZERO,PL(0,1),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1
     $ ,1))
      CALL MP_FFV1L1_2(PL(0,1),W(1,2),GC_5,MDL_MB,ZERO,PL(0,2),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1
     $ ,2))
      CALL MP_FFS1L1_2(PL(0,2),W(1,4),GC_33,MDL_MB,ZERO,PL(0,3),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,3))
      CALL MP_FFS1L1_2(PL(0,3),W(1,5),GC_33,MDL_MB,ZERO,PL(0,4),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,3),4,COEFS,4,4,WL(1,0,1
     $ ,4))
      CALL MP_FFS1L1_2(PL(0,4),W(1,3),GC_33,MDL_MB,ZERO,PL(0,5),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,4),4,COEFS,4,4,WL(1,0,1
     $ ,5))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,5),5,4,1,1,1,15)
C     Coefficient construction for loop diagram with ID 2
      CALL MP_FFS1L1_2(PL(0,2),W(1,5),GC_33,MDL_MB,ZERO,PL(0,6),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,6))
      CALL MP_FFS1L1_2(PL(0,6),W(1,4),GC_33,MDL_MB,ZERO,PL(0,7),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,6),4,COEFS,4,4,WL(1,0,1
     $ ,7))
      CALL MP_FFS1L1_2(PL(0,7),W(1,3),GC_33,MDL_MB,ZERO,PL(0,8),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1
     $ ,8))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,8),5,4,2,1,1,16)
C     Coefficient construction for loop diagram with ID 3
      CALL MP_FFS1L1_2(PL(0,2),W(1,3),GC_33,MDL_MB,ZERO,PL(0,9),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,9))
      CALL MP_FFS1L1_2(PL(0,9),W(1,5),GC_33,MDL_MB,ZERO,PL(0,10),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,9),4,COEFS,4,4,WL(1,0,1
     $ ,10))
      CALL MP_FFS1L1_2(PL(0,10),W(1,4),GC_33,MDL_MB,ZERO,PL(0,11)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,10),4,COEFS,4,4,WL(1,0
     $ ,1,11))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,11),5,4,3,1,1,17)
C     Coefficient construction for loop diagram with ID 4
      CALL MP_FFS1L1_2(PL(0,6),W(1,3),GC_33,MDL_MB,ZERO,PL(0,12),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,6),4,COEFS,4,4,WL(1,0,1
     $ ,12))
      CALL MP_FFS1L1_2(PL(0,12),W(1,4),GC_33,MDL_MB,ZERO,PL(0,13)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,12),4,COEFS,4,4,WL(1,0
     $ ,1,13))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,13),5,4,4,1,1,18)
C     Coefficient construction for loop diagram with ID 5
      CALL MP_FFS1L1_2(PL(0,9),W(1,4),GC_33,MDL_MB,ZERO,PL(0,14),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,9),4,COEFS,4,4,WL(1,0,1
     $ ,14))
      CALL MP_FFS1L1_2(PL(0,14),W(1,5),GC_33,MDL_MB,ZERO,PL(0,15)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,14),4,COEFS,4,4,WL(1,0
     $ ,1,15))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,15),5,4,5,1,1,19)
C     Coefficient construction for loop diagram with ID 6
      CALL MP_FFS1L1_2(PL(0,3),W(1,3),GC_33,MDL_MB,ZERO,PL(0,16),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,3),4,COEFS,4,4,WL(1,0,1
     $ ,16))
      CALL MP_FFS1L1_2(PL(0,16),W(1,5),GC_33,MDL_MB,ZERO,PL(0,17)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,16),4,COEFS,4,4,WL(1,0
     $ ,1,17))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,17),5,4,6,1,1,20)
C     Coefficient construction for loop diagram with ID 7
      CALL MP_FFS1L1_2(PL(0,2),W(1,7),GC_33,MDL_MB,ZERO,PL(0,18),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,18))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,18),3,4,7,1,1,21)
C     Coefficient construction for loop diagram with ID 8
      CALL MP_FFS1L1_2(PL(0,6),W(1,6),GC_33,MDL_MB,ZERO,PL(0,19),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,6),4,COEFS,4,4,WL(1,0,1
     $ ,19))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,19),4,4,8,1,1,22)
C     Coefficient construction for loop diagram with ID 9
      CALL MP_FFS1L1_2(PL(0,2),W(1,6),GC_33,MDL_MB,ZERO,PL(0,20),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,20))
      CALL MP_FFS1L1_2(PL(0,20),W(1,5),GC_33,MDL_MB,ZERO,PL(0,21)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,20),4,COEFS,4,4,WL(1,0
     $ ,1,21))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,21),4,4,9,1,1,23)
C     Coefficient construction for loop diagram with ID 10
      CALL MP_FFS1L1_2(PL(0,2),W(1,9),GC_33,MDL_MB,ZERO,PL(0,22),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,22))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,22),3,4,10,1,1,24)
C     Coefficient construction for loop diagram with ID 11
      CALL MP_FFS1L1_2(PL(0,3),W(1,8),GC_33,MDL_MB,ZERO,PL(0,23),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,3),4,COEFS,4,4,WL(1,0,1
     $ ,23))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,23),4,4,11,1,1,25)
C     Coefficient construction for loop diagram with ID 12
      CALL MP_FFS1L1_2(PL(0,2),W(1,8),GC_33,MDL_MB,ZERO,PL(0,24),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,24))
      CALL MP_FFS1L1_2(PL(0,24),W(1,4),GC_33,MDL_MB,ZERO,PL(0,25)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,24),4,COEFS,4,4,WL(1,0
     $ ,1,25))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,25),4,4,12,1,1,26)
C     Coefficient construction for loop diagram with ID 13
      CALL MP_FFS1L1_2(PL(0,2),W(1,11),GC_33,MDL_MB,ZERO,PL(0,26)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,26))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,26),3,4,13,1,1,27)
C     Coefficient construction for loop diagram with ID 14
      CALL MP_FFS1L1_2(PL(0,2),W(1,10),GC_33,MDL_MB,ZERO,PL(0,27)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,27))
      CALL MP_FFS1L1_2(PL(0,27),W(1,3),GC_33,MDL_MB,ZERO,PL(0,28)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,27),4,COEFS,4,4,WL(1,0
     $ ,1,28))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,28),4,4,14,1,1,28)
C     Coefficient construction for loop diagram with ID 15
      CALL MP_FFS1L1_2(PL(0,9),W(1,10),GC_33,MDL_MB,ZERO,PL(0,29)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,9),4,COEFS,4,4,WL(1,0,1
     $ ,29))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,29),4,4,15,1,1,29)
C     Coefficient construction for loop diagram with ID 16
      CALL MP_FFS1L1_2(PL(0,2),W(1,12),GC_33,MDL_MB,ZERO,PL(0,30)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,2),4,COEFS,4,4,WL(1,0,1
     $ ,30))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,30),3,4,16,1,1,30)
C     Coefficient construction for loop diagram with ID 17
      CALL MP_FFV1L2_1(PL(0,0),W(1,1),GC_5,MDL_MB,ZERO,PL(0,31),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1
     $ ,31))
      CALL MP_FFV1L2_1(PL(0,31),W(1,2),GC_5,MDL_MB,ZERO,PL(0,32),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,31),4,COEFS,4,4,WL(1,0
     $ ,1,32))
      CALL MP_FFS1L2_1(PL(0,32),W(1,7),GC_33,MDL_MB,ZERO,PL(0,33)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,33))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,33),3,4,17,1,1,31)
C     Coefficient construction for loop diagram with ID 18
      CALL MP_FFS1L2_1(PL(0,32),W(1,5),GC_33,MDL_MB,ZERO,PL(0,34)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,34))
      CALL MP_FFS1L2_1(PL(0,34),W(1,6),GC_33,MDL_MB,ZERO,PL(0,35)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,34),4,COEFS,4,4,WL(1,0
     $ ,1,35))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,35),4,4,18,1,1,32)
C     Coefficient construction for loop diagram with ID 19
      CALL MP_FFS1L2_1(PL(0,31),W(1,5),GC_33,MDL_MB,ZERO,PL(0,36)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,31),4,COEFS,4,4,WL(1,0
     $ ,1,36))
      CALL MP_FFV1L2_1(PL(0,36),W(1,2),GC_5,MDL_MB,ZERO,PL(0,37),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,36),4,COEFS,4,4,WL(1,0
     $ ,1,37))
      CALL MP_FFS1L2_1(PL(0,37),W(1,6),GC_33,MDL_MB,ZERO,PL(0,38)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,37),4,COEFS,4,4,WL(1,0
     $ ,1,38))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,38),4,4,19,1,1,33)
C     Coefficient construction for loop diagram with ID 20
      CALL MP_FFS1L2_1(PL(0,32),W(1,6),GC_33,MDL_MB,ZERO,PL(0,39)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,39))
      CALL MP_FFS1L2_1(PL(0,39),W(1,5),GC_33,MDL_MB,ZERO,PL(0,40)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,39),4,COEFS,4,4,WL(1,0
     $ ,1,40))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,40),4,4,20,1,1,34)
C     Coefficient construction for loop diagram with ID 21
      CALL MP_FFS1L1_2(PL(0,1),W(1,5),GC_33,MDL_MB,ZERO,PL(0,41),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1
     $ ,41))
      CALL MP_FFV1L1_2(PL(0,41),W(1,2),GC_5,MDL_MB,ZERO,PL(0,42),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,41),4,COEFS,4,4,WL(1,0
     $ ,1,42))
      CALL MP_FFS1L1_2(PL(0,42),W(1,6),GC_33,MDL_MB,ZERO,PL(0,43)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0
     $ ,1,43))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,43),4,4,21,1,1,35)
C     Coefficient construction for loop diagram with ID 22
      CALL MP_FFS1L2_1(PL(0,32),W(1,9),GC_33,MDL_MB,ZERO,PL(0,44)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,44))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,44),3,4,22,1,1,36)
C     Coefficient construction for loop diagram with ID 23
      CALL MP_FFS1L2_1(PL(0,32),W(1,4),GC_33,MDL_MB,ZERO,PL(0,45)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,45))
      CALL MP_FFS1L2_1(PL(0,45),W(1,8),GC_33,MDL_MB,ZERO,PL(0,46)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,45),4,COEFS,4,4,WL(1,0
     $ ,1,46))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,46),4,4,23,1,1,37)
C     Coefficient construction for loop diagram with ID 24
      CALL MP_FFS1L2_1(PL(0,31),W(1,4),GC_33,MDL_MB,ZERO,PL(0,47)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,31),4,COEFS,4,4,WL(1,0
     $ ,1,47))
      CALL MP_FFV1L2_1(PL(0,47),W(1,2),GC_5,MDL_MB,ZERO,PL(0,48),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,47),4,COEFS,4,4,WL(1,0
     $ ,1,48))
      CALL MP_FFS1L2_1(PL(0,48),W(1,8),GC_33,MDL_MB,ZERO,PL(0,49)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,48),4,COEFS,4,4,WL(1,0
     $ ,1,49))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,49),4,4,24,1,1,38)
C     Coefficient construction for loop diagram with ID 25
      CALL MP_FFS1L2_1(PL(0,32),W(1,8),GC_33,MDL_MB,ZERO,PL(0,50)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,50))
      CALL MP_FFS1L2_1(PL(0,50),W(1,4),GC_33,MDL_MB,ZERO,PL(0,51)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,50),4,COEFS,4,4,WL(1,0
     $ ,1,51))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,51),4,4,25,1,1,39)
C     Coefficient construction for loop diagram with ID 26
      CALL MP_FFS1L1_2(PL(0,1),W(1,4),GC_33,MDL_MB,ZERO,PL(0,52),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1
     $ ,52))
      CALL MP_FFV1L1_2(PL(0,52),W(1,2),GC_5,MDL_MB,ZERO,PL(0,53),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,52),4,COEFS,4,4,WL(1,0
     $ ,1,53))
      CALL MP_FFS1L1_2(PL(0,53),W(1,8),GC_33,MDL_MB,ZERO,PL(0,54)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,53),4,COEFS,4,4,WL(1,0
     $ ,1,54))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,54),4,4,26,1,1,40)
C     Coefficient construction for loop diagram with ID 27
      CALL MP_FFS1L2_1(PL(0,34),W(1,4),GC_33,MDL_MB,ZERO,PL(0,55)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,34),4,COEFS,4,4,WL(1,0
     $ ,1,55))
      CALL MP_FFS1L2_1(PL(0,55),W(1,3),GC_33,MDL_MB,ZERO,PL(0,56)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,55),4,COEFS,4,4,WL(1,0
     $ ,1,56))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,56),5,4,27,1,1,41)
C     Coefficient construction for loop diagram with ID 28
      CALL MP_FFS1L2_1(PL(0,45),W(1,5),GC_33,MDL_MB,ZERO,PL(0,57)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,45),4,COEFS,4,4,WL(1,0
     $ ,1,57))
      CALL MP_FFS1L2_1(PL(0,57),W(1,3),GC_33,MDL_MB,ZERO,PL(0,58)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,57),4,COEFS,4,4,WL(1,0
     $ ,1,58))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,58),5,4,28,1,1,42)
C     Coefficient construction for loop diagram with ID 29
      CALL MP_FFS1L1_2(PL(0,1),W(1,3),GC_33,MDL_MB,ZERO,PL(0,59),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1
     $ ,59))
      CALL MP_FFV1L1_2(PL(0,59),W(1,2),GC_5,MDL_MB,ZERO,PL(0,60),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,59),4,COEFS,4,4,WL(1,0
     $ ,1,60))
      CALL MP_FFS1L1_2(PL(0,60),W(1,5),GC_33,MDL_MB,ZERO,PL(0,61)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,60),4,COEFS,4,4,WL(1,0
     $ ,1,61))
      CALL MP_FFS1L1_2(PL(0,61),W(1,4),GC_33,MDL_MB,ZERO,PL(0,62)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,61),4,COEFS,4,4,WL(1,0
     $ ,1,62))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,62),5,4,29,1,1,43)
C     Coefficient construction for loop diagram with ID 30
      CALL MP_FFS1L1_2(PL(0,59),W(1,5),GC_33,MDL_MB,ZERO,PL(0,63)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,59),4,COEFS,4,4,WL(1,0
     $ ,1,63))
      CALL MP_FFV1L1_2(PL(0,63),W(1,2),GC_5,MDL_MB,ZERO,PL(0,64),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0
     $ ,1,64))
      CALL MP_FFS1L1_2(PL(0,64),W(1,4),GC_33,MDL_MB,ZERO,PL(0,65)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,64),4,COEFS,4,4,WL(1,0
     $ ,1,65))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,65),5,4,30,1,1,44)
C     Coefficient construction for loop diagram with ID 31
      CALL MP_FFS1L1_2(PL(0,60),W(1,4),GC_33,MDL_MB,ZERO,PL(0,66)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,60),4,COEFS,4,4,WL(1,0
     $ ,1,66))
      CALL MP_FFS1L1_2(PL(0,66),W(1,5),GC_33,MDL_MB,ZERO,PL(0,67)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,66),4,COEFS,4,4,WL(1,0
     $ ,1,67))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,67),5,4,31,1,1,45)
C     Coefficient construction for loop diagram with ID 32
      CALL MP_FFS1L1_2(PL(0,59),W(1,4),GC_33,MDL_MB,ZERO,PL(0,68)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,59),4,COEFS,4,4,WL(1,0
     $ ,1,68))
      CALL MP_FFV1L1_2(PL(0,68),W(1,2),GC_5,MDL_MB,ZERO,PL(0,69),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,68),4,COEFS,4,4,WL(1,0
     $ ,1,69))
      CALL MP_FFS1L1_2(PL(0,69),W(1,5),GC_33,MDL_MB,ZERO,PL(0,70)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,69),4,COEFS,4,4,WL(1,0
     $ ,1,70))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,70),5,4,32,1,1,46)
C     Coefficient construction for loop diagram with ID 33
      CALL MP_FFS1L2_1(PL(0,32),W(1,10),GC_33,MDL_MB,ZERO,PL(0,71)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,71))
      CALL MP_FFS1L2_1(PL(0,71),W(1,3),GC_33,MDL_MB,ZERO,PL(0,72)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,71),4,COEFS,4,4,WL(1,0
     $ ,1,72))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,72),4,4,33,1,1,47)
C     Coefficient construction for loop diagram with ID 34
      CALL MP_FFS1L1_2(PL(0,60),W(1,10),GC_33,MDL_MB,ZERO,PL(0,73)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,60),4,COEFS,4,4,WL(1,0
     $ ,1,73))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,73),4,4,34,1,1,48)
C     Coefficient construction for loop diagram with ID 35
      CALL MP_FFS1L2_1(PL(0,32),W(1,11),GC_33,MDL_MB,ZERO,PL(0,74)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,74))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,74),3,4,35,1,1,49)
C     Coefficient construction for loop diagram with ID 36
      CALL MP_FFS1L2_1(PL(0,32),W(1,3),GC_33,MDL_MB,ZERO,PL(0,75)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,75))
      CALL MP_FFS1L2_1(PL(0,75),W(1,10),GC_33,MDL_MB,ZERO,PL(0,76)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,75),4,COEFS,4,4,WL(1,0
     $ ,1,76))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,76),4,4,36,1,1,50)
C     Coefficient construction for loop diagram with ID 37
      CALL MP_FFS1L2_1(PL(0,31),W(1,3),GC_33,MDL_MB,ZERO,PL(0,77)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,31),4,COEFS,4,4,WL(1,0
     $ ,1,77))
      CALL MP_FFV1L2_1(PL(0,77),W(1,2),GC_5,MDL_MB,ZERO,PL(0,78),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,77),4,COEFS,4,4,WL(1,0
     $ ,1,78))
      CALL MP_FFS1L2_1(PL(0,78),W(1,10),GC_33,MDL_MB,ZERO,PL(0,79)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0
     $ ,1,79))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,79),4,4,37,1,1,51)
C     Coefficient construction for loop diagram with ID 38
      CALL MP_FFS1L2_1(PL(0,34),W(1,3),GC_33,MDL_MB,ZERO,PL(0,80)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,34),4,COEFS,4,4,WL(1,0
     $ ,1,80))
      CALL MP_FFS1L2_1(PL(0,80),W(1,4),GC_33,MDL_MB,ZERO,PL(0,81)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,80),4,COEFS,4,4,WL(1,0
     $ ,1,81))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,81),5,4,38,1,1,52)
C     Coefficient construction for loop diagram with ID 39
      CALL MP_FFS1L2_1(PL(0,75),W(1,5),GC_33,MDL_MB,ZERO,PL(0,82)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,75),4,COEFS,4,4,WL(1,0
     $ ,1,82))
      CALL MP_FFS1L2_1(PL(0,82),W(1,4),GC_33,MDL_MB,ZERO,PL(0,83)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,82),4,COEFS,4,4,WL(1,0
     $ ,1,83))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,83),5,4,39,1,1,53)
C     Coefficient construction for loop diagram with ID 40
      CALL MP_FFS1L2_1(PL(0,77),W(1,5),GC_33,MDL_MB,ZERO,PL(0,84)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,77),4,COEFS,4,4,WL(1,0
     $ ,1,84))
      CALL MP_FFV1L2_1(PL(0,84),W(1,2),GC_5,MDL_MB,ZERO,PL(0,85),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,84),4,COEFS,4,4,WL(1,0
     $ ,1,85))
      CALL MP_FFS1L2_1(PL(0,85),W(1,4),GC_33,MDL_MB,ZERO,PL(0,86)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,85),4,COEFS,4,4,WL(1,0
     $ ,1,86))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,86),5,4,40,1,1,54)
C     Coefficient construction for loop diagram with ID 41
      CALL MP_FFS1L2_1(PL(0,78),W(1,5),GC_33,MDL_MB,ZERO,PL(0,87)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0
     $ ,1,87))
      CALL MP_FFS1L2_1(PL(0,87),W(1,4),GC_33,MDL_MB,ZERO,PL(0,88)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,87),4,COEFS,4,4,WL(1,0
     $ ,1,88))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,88),5,4,41,1,1,55)
C     Coefficient construction for loop diagram with ID 42
      CALL MP_FFS1L1_2(PL(0,53),W(1,3),GC_33,MDL_MB,ZERO,PL(0,89)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,53),4,COEFS,4,4,WL(1,0
     $ ,1,89))
      CALL MP_FFS1L1_2(PL(0,89),W(1,5),GC_33,MDL_MB,ZERO,PL(0,90)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,89),4,COEFS,4,4,WL(1,0
     $ ,1,90))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,90),5,4,42,1,1,56)
C     Coefficient construction for loop diagram with ID 43
      CALL MP_FFS1L1_2(PL(0,52),W(1,3),GC_33,MDL_MB,ZERO,PL(0,91)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,52),4,COEFS,4,4,WL(1,0
     $ ,1,91))
      CALL MP_FFV1L1_2(PL(0,91),W(1,2),GC_5,MDL_MB,ZERO,PL(0,92),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,91),4,COEFS,4,4,WL(1,0
     $ ,1,92))
      CALL MP_FFS1L1_2(PL(0,92),W(1,5),GC_33,MDL_MB,ZERO,PL(0,93)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,92),4,COEFS,4,4,WL(1,0
     $ ,1,93))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,93),5,4,43,1,1,57)
C     Coefficient construction for loop diagram with ID 44
      CALL MP_FFS1L2_1(PL(0,45),W(1,3),GC_33,MDL_MB,ZERO,PL(0,94)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,45),4,COEFS,4,4,WL(1,0
     $ ,1,94))
      CALL MP_FFS1L2_1(PL(0,94),W(1,5),GC_33,MDL_MB,ZERO,PL(0,95)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,94),4,COEFS,4,4,WL(1,0
     $ ,1,95))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,95),5,4,44,1,1,58)
C     Coefficient construction for loop diagram with ID 45
      CALL MP_FFS1L2_1(PL(0,75),W(1,4),GC_33,MDL_MB,ZERO,PL(0,96)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,75),4,COEFS,4,4,WL(1,0
     $ ,1,96))
      CALL MP_FFS1L2_1(PL(0,96),W(1,5),GC_33,MDL_MB,ZERO,PL(0,97)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,96),4,COEFS,4,4,WL(1,0
     $ ,1,97))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,97),5,4,45,1,1,59)
C     Coefficient construction for loop diagram with ID 46
      CALL MP_FFS1L2_1(PL(0,77),W(1,4),GC_33,MDL_MB,ZERO,PL(0,98)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,77),4,COEFS,4,4,WL(1,0
     $ ,1,98))
      CALL MP_FFV1L2_1(PL(0,98),W(1,2),GC_5,MDL_MB,ZERO,PL(0,99),COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,98),4,COEFS,4,4,WL(1,0
     $ ,1,99))
      CALL MP_FFS1L2_1(PL(0,99),W(1,5),GC_33,MDL_MB,ZERO,PL(0,100)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,99),4,COEFS,4,4,WL(1,0
     $ ,1,100))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,100),5,4,46,1,1,60)
C     Coefficient construction for loop diagram with ID 47
      CALL MP_FFS1L2_1(PL(0,78),W(1,4),GC_33,MDL_MB,ZERO,PL(0,101)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0
     $ ,1,101))
      CALL MP_FFS1L2_1(PL(0,101),W(1,5),GC_33,MDL_MB,ZERO,PL(0,102)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,101),4,COEFS,4,4,WL(1,0
     $ ,1,102))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,102),5,4,47,1,1,61)
C     Coefficient construction for loop diagram with ID 48
      CALL MP_FFS1L2_1(PL(0,47),W(1,3),GC_33,MDL_MB,ZERO,PL(0,103)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,47),4,COEFS,4,4,WL(1,0
     $ ,1,103))
      CALL MP_FFV1L2_1(PL(0,103),W(1,2),GC_5,MDL_MB,ZERO,PL(0,104)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,103),4,COEFS,4,4,WL(1,0
     $ ,1,104))
      CALL MP_FFS1L2_1(PL(0,104),W(1,5),GC_33,MDL_MB,ZERO,PL(0,105)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,104),4,COEFS,4,4,WL(1,0
     $ ,1,105))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,105),5,4,48,1,1,62)
C     Coefficient construction for loop diagram with ID 49
      CALL MP_FFS1L2_1(PL(0,48),W(1,3),GC_33,MDL_MB,ZERO,PL(0,106)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,48),4,COEFS,4,4,WL(1,0
     $ ,1,106))
      CALL MP_FFS1L2_1(PL(0,106),W(1,5),GC_33,MDL_MB,ZERO,PL(0,107)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,106),4,COEFS,4,4,WL(1,0
     $ ,1,107))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,107),5,4,49,1,1,63)
C     Coefficient construction for loop diagram with ID 50
      CALL MP_FFS1L2_1(PL(0,32),W(1,12),GC_33,MDL_MB,ZERO,PL(0,108)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,32),4,COEFS,4,4,WL(1,0
     $ ,1,108))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,108),3,4,50,1,1,64)
C     Coefficient construction for loop diagram with ID 51
      CALL MP_FFV1L1_2(PL(0,0),W(1,1),GC_5,MDL_MT,MDL_WT,PL(0,109)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1
     $ ,109))
      CALL MP_FFV1L1_2(PL(0,109),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,110)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,109),4,COEFS,4,4,WL(1,0
     $ ,1,110))
      CALL MP_FFS1L1_2(PL(0,110),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,111)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,111))
      CALL MP_FFS1L1_2(PL(0,111),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,112)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,111),4,COEFS,4,4,WL(1,0
     $ ,1,112))
      CALL MP_FFS1L1_2(PL(0,112),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,113)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,112),4,COEFS,4,4,WL(1,0
     $ ,1,113))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,113),5,4,51,1,1,65)
C     Coefficient construction for loop diagram with ID 52
      CALL MP_FFS1L1_2(PL(0,110),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,114)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,114))
      CALL MP_FFS1L1_2(PL(0,114),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,115)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,114),4,COEFS,4,4,WL(1,0
     $ ,1,115))
      CALL MP_FFS1L1_2(PL(0,115),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,116)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,115),4,COEFS,4,4,WL(1,0
     $ ,1,116))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,116),5,4,52,1,1,66)
C     Coefficient construction for loop diagram with ID 53
      CALL MP_FFS1L1_2(PL(0,110),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,117)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,117))
      CALL MP_FFS1L1_2(PL(0,117),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,118)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,117),4,COEFS,4,4,WL(1,0
     $ ,1,118))
      CALL MP_FFS1L1_2(PL(0,118),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,119)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,118),4,COEFS,4,4,WL(1,0
     $ ,1,119))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,119),5,4,53,1,1,67)
C     Coefficient construction for loop diagram with ID 54
      CALL MP_FFS1L1_2(PL(0,114),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,120)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,114),4,COEFS,4,4,WL(1,0
     $ ,1,120))
      CALL MP_FFS1L1_2(PL(0,120),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,121)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0
     $ ,1,121))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,121),5,4,54,1,1,68)
C     Coefficient construction for loop diagram with ID 55
      CALL MP_FFS1L1_2(PL(0,117),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,122)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,117),4,COEFS,4,4,WL(1,0
     $ ,1,122))
      CALL MP_FFS1L1_2(PL(0,122),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,123)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,122),4,COEFS,4,4,WL(1,0
     $ ,1,123))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,123),5,4,55,1,1,69)
C     Coefficient construction for loop diagram with ID 56
      CALL MP_FFS1L1_2(PL(0,111),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,124)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,111),4,COEFS,4,4,WL(1,0
     $ ,1,124))
      CALL MP_FFS1L1_2(PL(0,124),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,125)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,124),4,COEFS,4,4,WL(1,0
     $ ,1,125))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,125),5,4,56,1,1,70)
C     Coefficient construction for loop diagram with ID 57
      CALL MP_FFS1L1_2(PL(0,110),W(1,7),GC_37,MDL_MT,MDL_WT,PL(0,126)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,126))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,126),3,4,57,1,1,71)
C     Coefficient construction for loop diagram with ID 58
      CALL MP_FFS1L1_2(PL(0,114),W(1,6),GC_37,MDL_MT,MDL_WT,PL(0,127)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,114),4,COEFS,4,4,WL(1,0
     $ ,1,127))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,127),4,4,58,1,1,72)
C     Coefficient construction for loop diagram with ID 59
      CALL MP_FFS1L1_2(PL(0,110),W(1,6),GC_37,MDL_MT,MDL_WT,PL(0,128)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,128))
      CALL MP_FFS1L1_2(PL(0,128),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,129)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,128),4,COEFS,4,4,WL(1,0
     $ ,1,129))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,129),4,4,59,1,1,73)
C     Coefficient construction for loop diagram with ID 60
      CALL MP_FFS1L1_2(PL(0,110),W(1,9),GC_37,MDL_MT,MDL_WT,PL(0,130)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,130))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,130),3,4,60,1,1,74)
C     Coefficient construction for loop diagram with ID 61
      CALL MP_FFS1L1_2(PL(0,111),W(1,8),GC_37,MDL_MT,MDL_WT,PL(0,131)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,111),4,COEFS,4,4,WL(1,0
     $ ,1,131))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,131),4,4,61,1,1,75)
C     Coefficient construction for loop diagram with ID 62
      CALL MP_FFS1L1_2(PL(0,110),W(1,8),GC_37,MDL_MT,MDL_WT,PL(0,132)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,132))
      CALL MP_FFS1L1_2(PL(0,132),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,133)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,132),4,COEFS,4,4,WL(1,0
     $ ,1,133))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,133),4,4,62,1,1,76)
C     Coefficient construction for loop diagram with ID 63
      CALL MP_FFS1L1_2(PL(0,110),W(1,11),GC_37,MDL_MT,MDL_WT,PL(0,134)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,134))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,134),3,4,63,1,1,77)
C     Coefficient construction for loop diagram with ID 64
      CALL MP_FFS1L1_2(PL(0,110),W(1,10),GC_37,MDL_MT,MDL_WT,PL(0,135)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,135))
      CALL MP_FFS1L1_2(PL(0,135),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,136)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,135),4,COEFS,4,4,WL(1,0
     $ ,1,136))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,136),4,4,64,1,1,78)
C     Coefficient construction for loop diagram with ID 65
      CALL MP_FFS1L1_2(PL(0,117),W(1,10),GC_37,MDL_MT,MDL_WT,PL(0,137)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,117),4,COEFS,4,4,WL(1,0
     $ ,1,137))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,137),4,4,65,1,1,79)
C     Coefficient construction for loop diagram with ID 66
      CALL MP_FFS1L1_2(PL(0,110),W(1,12),GC_37,MDL_MT,MDL_WT,PL(0,138)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,110),4,COEFS,4,4,WL(1,0
     $ ,1,138))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,138),3,4,66,1,1,80)
C     Coefficient construction for loop diagram with ID 67
      CALL MP_FFV1L2_1(PL(0,0),W(1,1),GC_5,MDL_MT,MDL_WT,PL(0,139)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1
     $ ,139))
      CALL MP_FFV1L2_1(PL(0,139),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,140)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,139),4,COEFS,4,4,WL(1,0
     $ ,1,140))
      CALL MP_FFS1L2_1(PL(0,140),W(1,7),GC_37,MDL_MT,MDL_WT,PL(0,141)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,141))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,141),3,4,67,1,1,81)
C     Coefficient construction for loop diagram with ID 68
      CALL MP_FFS1L2_1(PL(0,140),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,142)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,142))
      CALL MP_FFS1L2_1(PL(0,142),W(1,6),GC_37,MDL_MT,MDL_WT,PL(0,143)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,142),4,COEFS,4,4,WL(1,0
     $ ,1,143))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,143),4,4,68,1,1,82)
C     Coefficient construction for loop diagram with ID 69
      CALL MP_FFS1L2_1(PL(0,139),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,144)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,139),4,COEFS,4,4,WL(1,0
     $ ,1,144))
      CALL MP_FFV1L2_1(PL(0,144),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,145)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,144),4,COEFS,4,4,WL(1,0
     $ ,1,145))
      CALL MP_FFS1L2_1(PL(0,145),W(1,6),GC_37,MDL_MT,MDL_WT,PL(0,146)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,145),4,COEFS,4,4,WL(1,0
     $ ,1,146))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,146),4,4,69,1,1,83)
C     Coefficient construction for loop diagram with ID 70
      CALL MP_FFS1L2_1(PL(0,140),W(1,6),GC_37,MDL_MT,MDL_WT,PL(0,147)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,147))
      CALL MP_FFS1L2_1(PL(0,147),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,148)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,147),4,COEFS,4,4,WL(1,0
     $ ,1,148))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,148),4,4,70,1,1,84)
C     Coefficient construction for loop diagram with ID 71
      CALL MP_FFS1L1_2(PL(0,109),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,149)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,109),4,COEFS,4,4,WL(1,0
     $ ,1,149))
      CALL MP_FFV1L1_2(PL(0,149),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,150)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,149),4,COEFS,4,4,WL(1,0
     $ ,1,150))
      CALL MP_FFS1L1_2(PL(0,150),W(1,6),GC_37,MDL_MT,MDL_WT,PL(0,151)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,150),4,COEFS,4,4,WL(1,0
     $ ,1,151))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,151),4,4,71,1,1,85)
C     Coefficient construction for loop diagram with ID 72
      CALL MP_FFS1L2_1(PL(0,140),W(1,9),GC_37,MDL_MT,MDL_WT,PL(0,152)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,152))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,152),3,4,72,1,1,86)
C     Coefficient construction for loop diagram with ID 73
      CALL MP_FFS1L2_1(PL(0,140),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,153)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,153))
      CALL MP_FFS1L2_1(PL(0,153),W(1,8),GC_37,MDL_MT,MDL_WT,PL(0,154)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,153),4,COEFS,4,4,WL(1,0
     $ ,1,154))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,154),4,4,73,1,1,87)
C     Coefficient construction for loop diagram with ID 74
      CALL MP_FFS1L2_1(PL(0,139),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,155)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,139),4,COEFS,4,4,WL(1,0
     $ ,1,155))
      CALL MP_FFV1L2_1(PL(0,155),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,156)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,155),4,COEFS,4,4,WL(1,0
     $ ,1,156))
      CALL MP_FFS1L2_1(PL(0,156),W(1,8),GC_37,MDL_MT,MDL_WT,PL(0,157)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,156),4,COEFS,4,4,WL(1,0
     $ ,1,157))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,157),4,4,74,1,1,88)
C     Coefficient construction for loop diagram with ID 75
      CALL MP_FFS1L2_1(PL(0,140),W(1,8),GC_37,MDL_MT,MDL_WT,PL(0,158)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,158))
      CALL MP_FFS1L2_1(PL(0,158),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,159)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,158),4,COEFS,4,4,WL(1,0
     $ ,1,159))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,159),4,4,75,1,1,89)
C     Coefficient construction for loop diagram with ID 76
      CALL MP_FFS1L1_2(PL(0,109),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,160)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,109),4,COEFS,4,4,WL(1,0
     $ ,1,160))
      CALL MP_FFV1L1_2(PL(0,160),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,161)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,160),4,COEFS,4,4,WL(1,0
     $ ,1,161))
      CALL MP_FFS1L1_2(PL(0,161),W(1,8),GC_37,MDL_MT,MDL_WT,PL(0,162)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,161),4,COEFS,4,4,WL(1,0
     $ ,1,162))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,162),4,4,76,1,1,90)
C     Coefficient construction for loop diagram with ID 77
      CALL MP_FFS1L2_1(PL(0,142),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,163)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,142),4,COEFS,4,4,WL(1,0
     $ ,1,163))
      CALL MP_FFS1L2_1(PL(0,163),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,164)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,163),4,COEFS,4,4,WL(1,0
     $ ,1,164))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,164),5,4,77,1,1,91)
C     Coefficient construction for loop diagram with ID 78
      CALL MP_FFS1L2_1(PL(0,153),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,165)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,153),4,COEFS,4,4,WL(1,0
     $ ,1,165))
      CALL MP_FFS1L2_1(PL(0,165),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,166)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,165),4,COEFS,4,4,WL(1,0
     $ ,1,166))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,166),5,4,78,1,1,92)
C     Coefficient construction for loop diagram with ID 79
      CALL MP_FFS1L1_2(PL(0,109),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,167)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,109),4,COEFS,4,4,WL(1,0
     $ ,1,167))
      CALL MP_FFV1L1_2(PL(0,167),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,168)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,167),4,COEFS,4,4,WL(1,0
     $ ,1,168))
      CALL MP_FFS1L1_2(PL(0,168),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,169)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,168),4,COEFS,4,4,WL(1,0
     $ ,1,169))
      CALL MP_FFS1L1_2(PL(0,169),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,170)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,169),4,COEFS,4,4,WL(1,0
     $ ,1,170))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,170),5,4,79,1,1,93)
C     Coefficient construction for loop diagram with ID 80
      CALL MP_FFS1L1_2(PL(0,167),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,171)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,167),4,COEFS,4,4,WL(1,0
     $ ,1,171))
      CALL MP_FFV1L1_2(PL(0,171),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,172)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,171),4,COEFS,4,4,WL(1,0
     $ ,1,172))
      CALL MP_FFS1L1_2(PL(0,172),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,173)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,172),4,COEFS,4,4,WL(1,0
     $ ,1,173))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,173),5,4,80,1,1,94)
C     Coefficient construction for loop diagram with ID 81
      CALL MP_FFS1L1_2(PL(0,168),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,174)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,168),4,COEFS,4,4,WL(1,0
     $ ,1,174))
      CALL MP_FFS1L1_2(PL(0,174),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,175)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,174),4,COEFS,4,4,WL(1,0
     $ ,1,175))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,175),5,4,81,1,1,95)
C     Coefficient construction for loop diagram with ID 82
      CALL MP_FFS1L1_2(PL(0,167),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,176)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,167),4,COEFS,4,4,WL(1,0
     $ ,1,176))
      CALL MP_FFV1L1_2(PL(0,176),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,177)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,176),4,COEFS,4,4,WL(1,0
     $ ,1,177))
      CALL MP_FFS1L1_2(PL(0,177),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,178)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,177),4,COEFS,4,4,WL(1,0
     $ ,1,178))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,178),5,4,82,1,1,96)
C     Coefficient construction for loop diagram with ID 83
      CALL MP_FFS1L2_1(PL(0,140),W(1,10),GC_37,MDL_MT,MDL_WT,PL(0,179)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,179))
      CALL MP_FFS1L2_1(PL(0,179),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,180)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,179),4,COEFS,4,4,WL(1,0
     $ ,1,180))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,180),4,4,83,1,1,97)
C     Coefficient construction for loop diagram with ID 84
      CALL MP_FFS1L1_2(PL(0,168),W(1,10),GC_37,MDL_MT,MDL_WT,PL(0,181)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,168),4,COEFS,4,4,WL(1,0
     $ ,1,181))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,181),4,4,84,1,1,98)
C     Coefficient construction for loop diagram with ID 85
      CALL MP_FFS1L2_1(PL(0,140),W(1,11),GC_37,MDL_MT,MDL_WT,PL(0,182)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,182))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,182),3,4,85,1,1,99)
C     Coefficient construction for loop diagram with ID 86
      CALL MP_FFS1L2_1(PL(0,140),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,183)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,183))
      CALL MP_FFS1L2_1(PL(0,183),W(1,10),GC_37,MDL_MT,MDL_WT,PL(0,184)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,183),4,COEFS,4,4,WL(1,0
     $ ,1,184))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,184),4,4,86,1,1,100)
C     Coefficient construction for loop diagram with ID 87
      CALL MP_FFS1L2_1(PL(0,139),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,185)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_1_1(WL(1,0,1,139),4,COEFS,4,4,WL(1,0
     $ ,1,185))
      CALL MP_FFV1L2_1(PL(0,185),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,186)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,185),4,COEFS,4,4,WL(1,0
     $ ,1,186))
      CALL MP_FFS1L2_1(PL(0,186),W(1,10),GC_37,MDL_MT,MDL_WT,PL(0,187)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,186),4,COEFS,4,4,WL(1,0
     $ ,1,187))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,187),4,4,87,1,1,101)
C     Coefficient construction for loop diagram with ID 88
      CALL MP_FFS1L2_1(PL(0,142),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,188)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,142),4,COEFS,4,4,WL(1,0
     $ ,1,188))
      CALL MP_FFS1L2_1(PL(0,188),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,189)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,188),4,COEFS,4,4,WL(1,0
     $ ,1,189))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,189),5,4,88,1,1,102)
C     Coefficient construction for loop diagram with ID 89
      CALL MP_FFS1L2_1(PL(0,183),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,190)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,183),4,COEFS,4,4,WL(1,0
     $ ,1,190))
      CALL MP_FFS1L2_1(PL(0,190),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,191)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,190),4,COEFS,4,4,WL(1,0
     $ ,1,191))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,191),5,4,89,1,1,103)
C     Coefficient construction for loop diagram with ID 90
      CALL MP_FFS1L2_1(PL(0,185),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,192)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,185),4,COEFS,4,4,WL(1,0
     $ ,1,192))
      CALL MP_FFV1L2_1(PL(0,192),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,193)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,192),4,COEFS,4,4,WL(1,0
     $ ,1,193))
      CALL MP_FFS1L2_1(PL(0,193),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,194)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,193),4,COEFS,4,4,WL(1,0
     $ ,1,194))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,194),5,4,90,1,1,104)
C     Coefficient construction for loop diagram with ID 91
      CALL MP_FFS1L2_1(PL(0,186),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,195)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,186),4,COEFS,4,4,WL(1,0
     $ ,1,195))
      CALL MP_FFS1L2_1(PL(0,195),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,196)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,195),4,COEFS,4,4,WL(1,0
     $ ,1,196))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,196),5,4,91,1,1,105)
C     Coefficient construction for loop diagram with ID 92
      CALL MP_FFS1L1_2(PL(0,161),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,197)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,161),4,COEFS,4,4,WL(1,0
     $ ,1,197))
      CALL MP_FFS1L1_2(PL(0,197),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,198)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,197),4,COEFS,4,4,WL(1,0
     $ ,1,198))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,198),5,4,92,1,1,106)
C     Coefficient construction for loop diagram with ID 93
      CALL MP_FFS1L1_2(PL(0,160),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,199)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,160),4,COEFS,4,4,WL(1,0
     $ ,1,199))
      CALL MP_FFV1L1_2(PL(0,199),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,200)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,199),4,COEFS,4,4,WL(1,0
     $ ,1,200))
      CALL MP_FFS1L1_2(PL(0,200),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,201)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,200),4,COEFS,4,4,WL(1,0
     $ ,1,201))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,201),5,4,93,1,1,107)
C     Coefficient construction for loop diagram with ID 94
      CALL MP_FFS1L2_1(PL(0,153),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,202)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,153),4,COEFS,4,4,WL(1,0
     $ ,1,202))
      CALL MP_FFS1L2_1(PL(0,202),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,203)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,202),4,COEFS,4,4,WL(1,0
     $ ,1,203))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,203),5,4,94,1,1,108)
C     Coefficient construction for loop diagram with ID 95
      CALL MP_FFS1L2_1(PL(0,183),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,204)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,183),4,COEFS,4,4,WL(1,0
     $ ,1,204))
      CALL MP_FFS1L2_1(PL(0,204),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,205)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,204),4,COEFS,4,4,WL(1,0
     $ ,1,205))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,205),5,4,95,1,1,109)
C     Coefficient construction for loop diagram with ID 96
      CALL MP_FFS1L2_1(PL(0,185),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,206)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,185),4,COEFS,4,4,WL(1,0
     $ ,1,206))
      CALL MP_FFV1L2_1(PL(0,206),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,207)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,206),4,COEFS,4,4,WL(1,0
     $ ,1,207))
      CALL MP_FFS1L2_1(PL(0,207),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,208)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,207),4,COEFS,4,4,WL(1,0
     $ ,1,208))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,208),5,4,96,1,1,110)
C     Coefficient construction for loop diagram with ID 97
      CALL MP_FFS1L2_1(PL(0,186),W(1,4),GC_37,MDL_MT,MDL_WT,PL(0,209)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,186),4,COEFS,4,4,WL(1,0
     $ ,1,209))
      CALL MP_FFS1L2_1(PL(0,209),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,210)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,209),4,COEFS,4,4,WL(1,0
     $ ,1,210))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,210),5,4,97,1,1,111)
C     Coefficient construction for loop diagram with ID 98
      CALL MP_FFS1L2_1(PL(0,155),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,211)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,155),4,COEFS,4,4,WL(1,0
     $ ,1,211))
      CALL MP_FFV1L2_1(PL(0,211),W(1,2),GC_5,MDL_MT,MDL_WT,PL(0,212)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,211),4,COEFS,4,4,WL(1,0
     $ ,1,212))
      CALL MP_FFS1L2_1(PL(0,212),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,213)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,212),4,COEFS,4,4,WL(1,0
     $ ,1,213))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,213),5,4,98,1,1,112)
C     Coefficient construction for loop diagram with ID 99
      CALL MP_FFS1L2_1(PL(0,156),W(1,3),GC_37,MDL_MT,MDL_WT,PL(0,214)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_3_1(WL(1,0,1,156),4,COEFS,4,4,WL(1,0
     $ ,1,214))
      CALL MP_FFS1L2_1(PL(0,214),W(1,5),GC_37,MDL_MT,MDL_WT,PL(0,215)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_4_1(WL(1,0,1,214),4,COEFS,4,4,WL(1,0
     $ ,1,215))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,215),5,4,99,1,1,113)
C     Coefficient construction for loop diagram with ID 100
      CALL MP_FFS1L2_1(PL(0,140),W(1,12),GC_37,MDL_MT,MDL_WT,PL(0,216)
     $ ,COEFS)
      CALL MP_ML5_0_0_1_UPDATE_WL_2_1(WL(1,0,1,140),4,COEFS,4,4,WL(1,0
     $ ,1,216))
      CALL MP_ML5_0_0_1_CREATE_LOOP_COEFS(WL(1,0,1,216),3,4,100,1,1
     $ ,114)
C     At this point, all loop coefficients needed for (QCD=4), i.e. of
C      split order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 4000

      GOTO 1001
 4000 CONTINUE
      MP_LOOP_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

