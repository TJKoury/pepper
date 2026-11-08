!$Procedure ZZSECPRT ( Calculate dot terms for DPSPCE perturbation )

      SUBROUTINE ZZSECPRT ( ISYNFL, DG   , DEL   , XNI  , &
                            OMEGAO, ATIME, OMGDOT, XLI  , &
                            XFACT , XLDOT, XNDOT , XNDDT )

! $ Abstract
!
!    Routine to calculate the dot terms for the secular perturbation
!    of a vehicle.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SECULAR PERTURBATION
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               ISYNFL

      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      ATIME
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      XLI
      DOUBLE PRECISION      DG     ( 10 )
      DOUBLE PRECISION      XFACT
      DOUBLE PRECISION      XLDOT
      DOUBLE PRECISION      XNI
      DOUBLE PRECISION      DEL    (  3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ISYNFL     I   Resonance flag
!     DG         I   Parameter array
!     DEL        I   Parameter array of delta values
!     XNI        I   An intermediate linear term passed from the
!                    calling routine
!     OMEGAO     I   Original argument of perigee
!     ATIME      I   An intermediate time term passed from the calling
!                    routine
!     OMGDOT     I   Time rate of change of argument of perigee
!     XLI        I   An intermediate angular term passed from the
!                    calling routine
!     XFACT      I   The value BFACT - XNQ
!     XLDOT      O   Time rate of change of XL
!     XNDOT      O   Time rate of change of XN
!     XNDDT      O   Time rate of change of XNDOT
!
! $ Detailed_Input
!
!     ISYNFL      is the flag used to indicate the need for resonance
!                 calculations.
!
!     DG          is the parameter array replacing the Dxxxx values.
!
!     DEL         is the parameter array replacing DEL1, DEL2 and DEL3.
!
!     XNI         is an intermediate linear term passed from the main
!                 term for the calculation of XLDOT = XNI  +  XFACT
!
!     OMEGAO      is the original value for the argument of perigee.
!
!     ATIME       is an intermediate time term passed from the main
!                 routine used to calculate the time dependent
!                 argument of perigee term XOMI
!
!     OMGDOT      is the time derivative of the argument of the perigee.
!
!     XLI         is an intermediate angular term
!
!     XFACT       is the value BFACT - XNQ calculated in ZZDPINIT
!
! $ Detailed_Output
!
!     XLDOT       time derivative of the XL term.
!
!     XNDOT       time derivative of the XN term.
!
!     XNDDT       second time derivative of XN, time derivative of the
!                 time derivative.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This subroutine was constructed from a section of code in ZZDPSEC
!     in the original Spacetrack 3 report.  The code block was called
!     using a set of conditional GO TO's.  The block has been written as
!     this subroutine to improve clarity and maintainability and to
!     conform to the NAIF style standard.
!
! $ Examples
!
!     None needed.
!
! $ Restrictions
!
!     1)  This routine should be called only by ZZDPSEC as part of the
!     DPSPCE subroutine package.  It has no other use.
!
! $ Literature_References
!
!     Spacetrack 3 report.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!
! $ Version
!
!-   SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!       Reordered header sections. Corrected typo in comments.
!
!-   SPICELIB Version 1.1.0, 24-MAR-1999 (EDW)
!
!       Correction made to format of Version descriptions.
!       Name of variable OMGDT changed to OMGDOT to be consistent
!       with name usage in other deep space two line elements
!       routines.
!
!-   SPICELIB Version 1.0.0, MAY-19-1997 (EDW)
!
!
!-&

! $ Index_Entries
!
!     perturbed dot terms
!
!-&

!
!     Local variables.
!
      DOUBLE PRECISION      X2LI
      DOUBLE PRECISION      X2OMI
      DOUBLE PRECISION      XNDDT
      DOUBLE PRECISION      XNDOT
      DOUBLE PRECISION      XOMI

      DOUBLE PRECISION      G22
      PARAMETER           ( G22    = 5.7686396D0  )

      DOUBLE PRECISION      G32
      PARAMETER           ( G32    = 0.95240898D0 )

      DOUBLE PRECISION      G44
      PARAMETER           ( G44    = 1.8014998D0  )

      DOUBLE PRECISION      G52
      PARAMETER           ( G52    = 1.0508330D0  )

      DOUBLE PRECISION      G54
      PARAMETER           ( G54    = 4.4108898D0  )

      DOUBLE PRECISION      FASX2
      PARAMETER           ( FASX2  = 0.13130908D0 )

      DOUBLE PRECISION      FASX4
      PARAMETER           ( FASX4  = 2.8843198D0  )

      DOUBLE PRECISION      FASX6
      PARAMETER           ( FASX6  = 0.37448087D0 )



!
!     Calculate the dot terms with respect to the state of the
!     resonance flag.
!

      IF ( ISYNFL .EQ. 0 ) THEN

!
!        Resonance flag set.
!

         XOMI  = OMEGAO + OMGDOT * ATIME
         X2OMI = XOMI   + XOMI
         X2LI  = XLI    + XLI

         XNDOT =  DG( 1  )*DSIN( X2OMI + XLI  - G22 ) &
                + DG( 2  )*DSIN( XLI          - G22 ) &
                + DG( 3  )*DSIN( XOMI  + XLI  - G32 ) &
                + DG( 4  )*DSIN(-XOMI  + XLI  - G32 ) &
                + DG( 5  )*DSIN( X2OMI + X2LI - G44 ) &
                + DG( 6  )*DSIN( X2LI         - G44 ) &
                + DG( 7  )*DSIN( XOMI  + XLI  - G52 ) &
                + DG( 8  )*DSIN(-XOMI  + XLI  - G52 ) &
                + DG( 9  )*DSIN( XOMI  + X2LI - G54 ) &
                + DG( 10 )*DSIN(-XOMI  + X2LI - G54 )

         XNDDT =  DG( 1  )*DCOS( X2OMI + XLI  - G22 ) &
                + DG( 2  )*DCOS( XLI          - G22 ) &
                + DG( 3  )*DCOS( XOMI  + XLI  - G32 ) &
                + DG( 4  )*DCOS(-XOMI  + XLI  - G32 ) &
                + DG( 7  )*DCOS( XOMI  + XLI  - G52 ) &
                + DG( 8  )*DCOS(-XOMI  + XLI  - G52 ) &
         +2.D0*(  DG( 5  )*DCOS( X2OMI + X2LI - G44 ) &
                + DG( 6  )*DCOS( X2LI         - G44 ) &
                + DG( 9  )*DCOS( XOMI  + X2LI - G54 ) &
                + DG( 10 )*DCOS( XOMI  + X2LI - G54 ))

      ELSE

!
!        Resonance flag not set
!

         XNDOT =  DEL(1)*DSIN(        XLI - FASX2   ) &
                + DEL(2)*DSIN( 2.D0*( XLI - FASX4 ) ) &
                + DEL(3)*DSIN( 3.D0*( XLI - FASX6 ) )

         XNDDT =      DEL(1)*DCOS(        XLI - FASX2   ) &
               + 2.D0*DEL(2)*DCOS( 2.D0*( XLI - FASX4 ) ) &
               + 3.D0*DEL(3)*DCOS( 3.D0*( XLI - FASX6 ) )

      END IF


      XLDOT = XNI  +  XFACT
      XNDDT = XNDDT * XLDOT

!
!     Hi!  What are you doing way down here?  Did you bring pizza?
!

      RETURN
      END
