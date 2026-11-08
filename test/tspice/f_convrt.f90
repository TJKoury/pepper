!$Procedure F_CONVRT ( CONVRT tests )

      SUBROUTINE F_CONVRT ( OK )

! $ Abstract
!
!     Exercise the SPICELIB unit conversion routine CONVRT.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine CONVRT.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 13-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TYEAR

      LOGICAL               EQSTR

!
!     Local Parameters
!

!
!     AU value from convrt.for, converted to km:
!
      DOUBLE PRECISION      AU
      PARAMETER           ( AU   = 1.4959787061368887D11 / 1.D3 )

!
!     Other constants from convrt.for:
!
      DOUBLE PRECISION      FEET
      PARAMETER           ( FEET   = 0.3048D0 )

      DOUBLE PRECISION      INCHES
      PARAMETER           ( INCHES = 0.0254D0 )

      DOUBLE PRECISION      SMILES
      PARAMETER           ( SMILES = 1609.344D0 )

      DOUBLE PRECISION      NMILES
      PARAMETER           ( NMILES = 1852.0D0 )

      DOUBLE PRECISION      YARDS
      PARAMETER           ( YARDS  = 0.9144D0 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               LBLLEN
      PARAMETER           ( LBLLEN = 50 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NDIST
      PARAMETER           ( NDIST  = 17 )

      INTEGER               NANG
      PARAMETER           ( NANG   = 7 )

      INTEGER               NTIME
      PARAMETER           ( NTIME  = 7 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    ANAMES ( NANG  )
      CHARACTER*(NAMLEN)    DNAMES ( NDIST )
      CHARACTER*(LBLLEN)    LABEL
      CHARACTER*(NAMLEN)    NAME
      CHARACTER*(NAMLEN)    TNAMES ( NTIME )

      DOUBLE PRECISION      ANGVAL ( NANG )
      DOUBLE PRECISION      DSTVAL ( NDIST )
      DOUBLE PRECISION      OUTVAL
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      TIMVAL ( NTIME )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XVAL

      INTEGER               I
      INTEGER               NUNIT

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!
      DATA                  ANAMES / 'radians', &
                                     'degrees', &
                                     'arcminutes', &
                                     'arcseconds', &
                                     'hourangle', &
                                     'minuteangle', &
                                     'secondangle'  /

      DATA                  DNAMES / 'meters', &
                                     'km', &
                                     'cm', &
                                     'mm', &
                                     'lightsecs', &
                                     'au', &
                                     'm', &
                                     'kilometers', &
                                     'centimeters', &
                                     'millimeters', &
                                     'feet', &
                                     'inches', &
                                     'statute_miles', &
                                     'nautical_miles', &
                                     'yards', &
                                     'lightyears', &
                                     'parsecs'        /

      DATA                  TNAMES / 'seconds', &
                                     'minutes', &
                                     'hours', &
                                     'days', &
                                     'julian_years', &
                                     'tropical_years', &
                                     'years'          /




!
!     Open the test family.
!
      CALL TOPEN ( 'F_CONVRT' )


!**********************************************************************
!
!     Normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup' )
!
!     Since we can call functions at run time, it's more convenient
!     to initialize some constants here than in a DATA statement.
!
!     Start with angular units.
!
!     We'll use RADIANs as our base value. Note that CONVRT
!     uses DEGREES.
!
      ANGVAL(1) = 1.D0
      ANGVAL(2) = RPD()
      ANGVAL(3) = ANGVAL(2) / 60.D0
      ANGVAL(4) = ANGVAL(3) / 60.D0
      ANGVAL(5) = 15.D0 * ANGVAL(2)
      ANGVAL(6) = ANGVAL(5) / 60.D0
      ANGVAL(7) = ANGVAL(6) / 60.D0

!
!     Initialize distance units. We'll use KM as our base unit.
!     Note that CONVRT uses meters.
!
      DSTVAL( 1) = 1.D-3
      DSTVAL( 2) = 1.D0
      DSTVAL( 3) = 1.D-5
      DSTVAL( 4) = 1.D-6
      DSTVAL( 5) = CLIGHT()
      DSTVAL( 6) = AU
      DSTVAL( 7) = 1.D-3
      DSTVAL( 8) = 1.D0
      DSTVAL( 9) = 1.D-5
      DSTVAL(10) = 1.D-6
      DSTVAL(11) = FEET   * 1.D-3
      DSTVAL(12) = INCHES * 1.D-3
      DSTVAL(13) = SMILES * 1.D-3
      DSTVAL(14) = NMILES * 1.D-3
      DSTVAL(15) = YARDS  * 1.D-3
      DSTVAL(16) = CLIGHT() * JYEAR()
      DSTVAL(17) = AU / SIN( RPD()/3600 )

!
!     Initialize time units. We'll use seconds as our base unit.
!
      TIMVAL(1) =     1.D0
      TIMVAL(2) =    60.D0
      TIMVAL(3) =  3600.D0
      TIMVAL(4) =    SPD()
      TIMVAL(5) =  JYEAR()
      TIMVAL(6) =  TYEAR()
      TIMVAL(7) =  JYEAR()



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert each time unit to seconds; use a ' &
      //           'non-identity input value.'                 )


      NUNIT = NTIME

      SCALE = 7.D0

      DO I = 1, NUNIT

         CALL CONVRT ( SCALE, TNAMES(I), 'SECONDS', OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Time units <#> in seconds.'
         CALL REPMC ( LABEL, '#', TNAMES(I), LABEL )

         XVAL = TIMVAL(I) * SCALE

         IF (  EQSTR(TNAMES(I), 'TROPICAL_YEARS') ) THEN

            TOL = 1.D-10
         ELSE
            TOL = VTIGHT
         END IF

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert each time unit to seconds; use a ' &
      //           'non-identity input value. Change case ' &
      //           'of input string.'                         )


      NUNIT = NTIME

      SCALE = 7.D0

      DO I = 1, NUNIT

         CALL UCASE ( TNAMES(I), NAME )

         CALL CONVRT ( SCALE, NAME, 'SECONDS', OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Time units <#> in seconds.'
         CALL REPMC ( LABEL, '#', TNAMES(I), LABEL )

         XVAL = TIMVAL(I) * SCALE

         IF (  EQSTR(TNAMES(I), 'TROPICAL_YEARS') ) THEN

            TOL = 1.D-10
         ELSE
            TOL = VTIGHT
         END IF

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert seconds to each time unit; use a ' &
      //           'non-identity input value.'                )


      NUNIT = NTIME

      SCALE = 1.D0/3.D0

      DO I = 1, NUNIT

         CALL CONVRT ( SCALE, 'SECONDS', TNAMES(I),  OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Seconds in time units <#>.'
         CALL REPMC ( LABEL, '#', TNAMES(I), LABEL )

         XVAL = SCALE / TIMVAL(I)

         IF (  EQSTR(TNAMES(I), 'TROPICAL_YEARS') ) THEN

            TOL = 1.D-10
         ELSE
            TOL = VTIGHT
         END IF

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert seconds to each time unit; use a ' &
      //           'non-identity input value. Change case of ' &
      //           'output string.'                           )


      NUNIT = NTIME

      SCALE = 1.D0/3.D0

      DO I = 1, NUNIT

         CALL UCASE ( TNAMES(I), NAME )

         CALL CONVRT ( SCALE, 'SECONDS', NAME,  OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Seconds in time units <#>.'
         CALL REPMC ( LABEL, '#', TNAMES(I), LABEL )

         XVAL = SCALE / TIMVAL(I)

         IF (  EQSTR(TNAMES(I), 'TROPICAL_YEARS') ) THEN

            TOL = 1.D-10
         ELSE
            TOL = VTIGHT
         END IF

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert each distance unit to km; use a ' &
      //           'non-identity input value.'                 )


      NUNIT = NDIST

      SCALE = 7.D0

      DO I = 1, NUNIT

         CALL CONVRT ( SCALE, DNAMES(I), 'KM', OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Distance units <#> in km.'
         CALL REPMC ( LABEL, '#', DNAMES(I), LABEL )

         XVAL = DSTVAL(I) * SCALE

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert each distance unit to km; use a ' &
      //           'non-identity input value. Change case of ' &
      //           'input string.'                            )


      NUNIT = NDIST

      SCALE = 7.D0

      DO I = 1, NUNIT

         CALL UCASE ( DNAMES(I), NAME )

         CALL CONVRT ( SCALE, NAME, 'KM', OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Distance units <#> in km.'
         CALL REPMC ( LABEL, '#', DNAMES(I), LABEL )

         XVAL = DSTVAL(I) * SCALE

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert km to each distance unit; use a ' &
      //           'non-identity input value.'                )


      NUNIT = NDIST

      SCALE = 1.D0/3.D0

      DO I = 1, NUNIT

         CALL CONVRT ( SCALE, 'KM', DNAMES(I),  OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'KM in distance units <#>.'
         CALL REPMC ( LABEL, '#', DNAMES(I), LABEL )

         XVAL = SCALE / DSTVAL(I)

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert km to each distance unit; use a ' &
      //           'non-identity input value. Change case of ' &
      //           'input string.'                            )


      NUNIT = NDIST

      SCALE = 1.D0/3.D0

      DO I = 1, NUNIT

         CALL UCASE ( DNAMES(I), NAME )

         CALL CONVRT ( SCALE, 'KM', NAME,  OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'KM in distance units <#>.'
         CALL REPMC ( LABEL, '#', DNAMES(I), LABEL )

         XVAL = SCALE / DSTVAL(I)

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert each angular unit to radians; use a ' &
      //           'non-identity input value.'                   )


      NUNIT = NANG

      SCALE = 11.D0

      DO I = 1, NUNIT

         CALL CONVRT ( SCALE, ANAMES(I), 'RADIANS', OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Angular units <#> in radians.'
         CALL REPMC ( LABEL, '#', ANAMES(I), LABEL )

         XVAL = ANGVAL(I) * SCALE

         TOL  = VTIGHT


         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert each angular unit to radians; use a ' &
      //           'non-identity input value. Change case of ' &
      //           'input string.'                            )


      NUNIT = NANG

      SCALE = 7.D0

      DO I = 1, NUNIT

         CALL UCASE ( ANAMES(I), NAME )

         CALL CONVRT ( SCALE, NAME, 'RADIANS', OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'Angular units <#> in radians.'
         CALL REPMC ( LABEL, '#', ANAMES(I), LABEL )

         XVAL = ANGVAL(I) * SCALE

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert radians to each angular unit; use a ' &
      //           'non-identity input value.'                )


      NUNIT = NANG

      SCALE = 1.D0/3.D0

      DO I = 1, NUNIT

         CALL CONVRT ( SCALE, 'RADIANS', ANAMES(I),  OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'RADIANS in angular units <#>.'
         CALL REPMC ( LABEL, '#', ANAMES(I), LABEL )

         XVAL = SCALE / ANGVAL(I)

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Convert radians to each angular unit; use a ' &
      //           'non-identity input value. Change case of ' &
      //           'input string.'                            )


      NUNIT = NANG

      SCALE = 1.D0/3.D0

      DO I = 1, NUNIT

         CALL UCASE ( ANAMES(I), NAME )

         CALL CONVRT ( SCALE, 'RADIANS', NAME,  OUTVAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LABEL = 'RADIANS in angular units <#>.'
         CALL REPMC ( LABEL, '#', ANAMES(I), LABEL )

         XVAL = SCALE / ANGVAL(I)

         TOL  = VTIGHT

         CALL CHCKSD ( LABEL, OUTVAL, '~/', XVAL, TOL, OK )

      END DO










!**********************************************************************
!
!     Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Incompatible units.' )


      CALL CONVRT ( 1.D0, 'RADIANS', 'KM', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)', OK )

      CALL CONVRT ( 1.D0, 'RADIANS', 'SECONDS', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)', OK )

      CALL CONVRT ( 1.D0, 'SECONDS', 'M', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)', OK )

      CALL CONVRT ( 1.D0, 'SECONDS', 'RADIANS', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)', OK )

      CALL CONVRT ( 1.D0, 'DEGREES', 'M', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)', OK )

      CALL CONVRT ( 1.D0, 'DEGREES', 'HOURS', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLEUNITS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized units.' )

      CALL CONVRT ( 1.D0, 'SECONDS', 'Z', OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSNOTREC)', OK )

      CALL CONVRT ( 1.D0, 'X', 'RADIANS',  OUTVAL )
      CALL CHCKXC ( .TRUE., 'SPICE(UNITSNOTREC)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


