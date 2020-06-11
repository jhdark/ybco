      SUBROUTINE MAGFLD (X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC)

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

      LOGICAL LFIRST
      CHARACTER BUFF
      INTEGER XI, YI, ZI

      PARAMETER (YBCO_INNER = 1.50375E-06)
      PARAMETER (YBCO_OUTER = 1.51375E-06)
      PARAMETER (WIRE_CURRENT = 3.79192e-04)
      PARAMETER (MU_0 = 4*PIPIPI*10e-07)
      PARAMETER (MU_COPPER = 4*PIPIPI*10e-07)
      PARAMETER (MU_STEEL = 1.02*MU_0)
      PARAMETER (MU_SILVER = 0.9998*MU_0)
      PARAMETER (MU_HASTALLOY = 1.0002*MU_0)
      DOUBLE PRECISION R
      
      R = SQRT(X**2 + Y**2)
      
      IF (R .GE. 0 .AND. R .LE. 0.75E-06) THEN
        MU = MU_COPPER
        B = MU*R*WIRE_CURRENT/2*PI*YBCO_INNER**2
      ELSEIF (R .GT. 0.75E-06 .AND. R .LE. 1.5E-06) THEN
        MU = MU_HSTALLOY
        B = MU*R*WIRE_CURRENT/2*PI*YBCO_INNER**2
      ELSEIF (R .GT. 1.5E-06 .AND. R .LE. YBCO_INNER) THEN
        MU = MU_0
        B = MU*R*WIRE_CURRENT/2*PI*YBCO_INNER**2
      ELSEIF (R .GT. YBCO_INNER .AND. R .LE. YBCO_OUTER) THEN
        MU = 0
        B = 0
      ELSEIF (R .GT. YBCO_OUTER .AND. R .LE. 1.52375E-06) THEN
        MU = MU_SILVER
        B = MU*WIRE_CURRENT/2*PI*R
      ELSEIF (R .GT. 1.52375E-06 .AND. R .LE. 2.7375E-06) THEN
        MU = MU_COPPER
        B = MU*WIRE_CURRENT/2*PI*R
      ELSEIF (R .GT. 2.27375E-06) THEN
        MU = MU_0
        B = MU*WIRE_CURRENT/2*PI*R
      ENDIF  
      
     
!     IF CURRENT FLOWING FROM +Z to -Z along wire
!     determine angle between the current x,y position and origin
      
      BTX = COS(X/R)
      BTY = SIN(X/R)       
      BTZ = 0

      RETURN
      END SUBROUTINE

