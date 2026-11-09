!$Procedure F_ZZDGDR ( ZZDAFGDR Test Family )

      SUBROUTINE F_ZZDGDR ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDAFGDR
!     routine.
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
!     This routine exercises ZZDAFGDR's logic.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 2.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    TSPICE Version 2.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    TSPICE Version 2.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    TSPICE Version 2.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    TSPICE Version 2.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    TSPICE Version 2.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    TSPICE Version 2.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    TSPICE Version 2.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 2.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 2.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 2.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    TSPICE Version 2.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    TSPICE Version 2.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    TSPICE Version 2.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    TSPICE Version 2.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    TSPICE Version 2.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    TSPICE Version 2.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    TSPICE Version 2.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    TSPICE Version 2.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    TSPICE Version 2.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    TSPICE Version 2.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    TSPICE Version 2.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
!
!        Updated module as the result of changes to the handle
!        manager interface, ZZDDHCLS.
!
!        Added MAC OS/X environments.
!
!-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!

!
!     Local Parameters
!

!
!     Local Variables
!
      CHARACTER*(80)        CASENM
      CHARACTER*(1024)      NAME
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(8)         IDWORD
      CHARACTER*(60)        IFNAME
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )

      DOUBLE PRECISION      ARRAY  (  128   )
      DOUBLE PRECISION      DATA   (  128   )
      DOUBLE PRECISION      DC     (  125   )
      DOUBLE PRECISION      RDREC  (  128   )

      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANLST ( NUMBFF )
      INTEGER               I
      INTEGER               IC     (  250   )
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               NATBFF
      INTEGER               ND
      INTEGER               NEXT
      INTEGER               NI
      INTEGER               NSUM
      INTEGER               NUMSUP
      INTEGER               PREV
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               UNIT

      LOGICAL               ADDFTP
      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_ZZDGDR' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_ZZDGDR Initialization' )

!
!     Retrieve the native format and other related information.
!
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

!
!     Setup the filename template.
!
      FNMTMP = 'daf#.daf'

!
!     Construct the contents of the DAF to create.
!
      IDWORD = 'DAF/TEST'
      ND     = 2
      NI     = 6
      IFNAME = 'TSPICE Test DAF'
      FWARD  = 2
      BWARD  = 2
      FREE   = 4*128+1
      ADDFTP = .FALSE.
      NEXT   = 0
      PREV   = 0
      NSUM   = 2

!
!     Create and pack the summaries.
!
      DC(1) = 1.0D0
      DC(2) = -1.0D0

      IC(1) = 1
      IC(2) = 2
      IC(3) = 3
      IC(4) = 4
      IC(5) = 3*128+1
      IC(6) = IC(5) + 64

      CALL DAFPS ( ND, NI, DC, IC, ARRAY )

      DC(1) = 2.0D0
      DC(2) = -2.0D0

      IC(1) = 2
      IC(2) = 4
      IC(3) = 6
      IC(4) = 8
      IC(5) = IC(6) + 1
      IC(6) = 4*128

      CALL DAFPS ( ND, NI, DC, IC, ARRAY( (ND+(NI+1)/2) + 1 ) )

!
!     Setup the name record.
!
      NAME = ' '

!
!     Populate the data record with a simple pattern.  Our
!     aim is not to validate the translation algorithm, but
!     the reader module.
!
      DO I = 1, 128
         DATA(I) = 10.0D0**I
      END DO

!
!     Construct the DAFs for each environment and load them
!     into the handle manager.
!
      DO I = 1, NUMSUP

         CALL REPMI    ( FNMTMP, '#', I, FNAME )

!
!        Start a new DAF.
!
         CALL T_DAFOPN ( FNAME, SUPBFF(I), UNIT )

!
!        Dump the file record to it.
!
         CALL T_DAFWFR ( UNIT,  SUPBFF(I), IDWORD, ND,    NI, IFNAME, &
                         FWARD, BWARD,     FREE,   ADDFTP             )

!
!        Now write the only summary record to the file.
!
         CALL T_DAFWSR ( UNIT, 2, SUPBFF(I), ND, NI, NEXT, PREV, NSUM, &
                         ARRAY                                      )

!
!        Insert a blank name record for completeness.
!
         WRITE ( UNIT = UNIT, REC = 3, IOSTAT = IOSTAT ) NAME
         CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

!
!        Lastly, dump the data record and close the file.
!
         CALL T_DAFWDR ( UNIT, 4, SUPBFF(I), 128, DATA )
         CLOSE ( UNIT = UNIT )

!
!        Open the DAF in the handle manager.
!
         CALL ZZDDHOPN ( FNAME, 'READ', 'DAF', HANLST(I) )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Dynamically construct the test cases based on the contents
!     of SUPBFF.
!
      DO I = 1, NUMSUP

         CASENM = '# reading # data.'
         CALL REPMC ( CASENM, '#', STRBFF(NATBFF),    CASENM )
         CALL REPMC ( CASENM, '#', STRBFF(SUPBFF(I)), CASENM )

         CALL TCASE ( CASENM )

!
!        Setup the inputs and outputs.
!
         DO J = 1, 128
            RDREC(J) = 0.0D0
         END DO

         FOUND = .FALSE.

!
!        We have all the pieces, invoke the module.
!
         CALL ZZDAFGDR ( HANLST(I), 4, RDREC, FOUND )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check outputs.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL CHCKAD ( 'RDREC', RDREC, '=', DATA, 128, 0.0D0, OK )

      END DO

!
!     Clean up by unloading and removing the test files.
!
      DO I = 1, NUMSUP
         CALL ZZDDHCLS ( HANLST(I), 'DAF', .FALSE. )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPICE(HANDLENOTFOUND) Exception' )

!
!     Setup outputs.
!
      FOUND = .TRUE.

      DO I = 1, 128
         RDREC(I) = DATA(I)
      END DO

!
!     Since we know we just unloaded HANLST(1), attempt to read
!     from that handle.
!
      CALL ZZDAFGDR ( HANLST(1), 4, RDREC, FOUND )

!
!     Check for the presence of an exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(HANDLENOTFOUND)', OK )

!
!     Check outputs. FOUND should be FALSE, and RDREC should be
!     untouched.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKAD ( 'RDREC', RDREC, '=', DATA, 128, 0.0D0, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
