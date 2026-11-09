!$Procedure T_BINGO ( BINGO: Process binary kernels to alternate BFFs )

      SUBROUTINE T_BINGO ( INAME, ONAME, OBFF )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Convert SPICE binary kernels from one supported binary file
!     format to another.
!
! $ Required_Reading
!
!     DAF
!     DAS
!
! $ Keywords
!
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE


      CHARACTER*(*)         INAME
      CHARACTER*(*)         ONAME
      INTEGER               OBFF

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INAME      I   Name of source binary kernel to convert.
!     ONAME      I   Name of output binary kernel to create.
!     OBFF       I   Integer code for binary file format of ONAME.
!
! $ Detailed_Input
!
!     INAME      is the name of the binary kernel to convert to
!                an alternate binary file format.
!
!     ONAME      is the name of the converted binary kernel to create.
!                The file named by ONAME will be destroyed and
!                replaced with the converted file.
!
!     OBFF       is an integer code that indicates the binary file
!                format targeted for ONAME.  Acceptable values are
!                the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the output file is already known to the handle manager
!         subsystem, the error SPICE(CANTCREATEFILE) is signaled.
!
!     2)  If the input file can't be opened, the error
!         SPICE(FILEOPENFAILED) is signaled.
!
!     3)  If the ID word of the input file can't be read, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     4)  If the input file's architecture is neither DAF nor DAS,
!         the error SPICE(UNKNOWNARCH) is signaled.
!
!     5)  Errors detected while converting the input file will be
!         diagnosed by routines in the call tree of this routine.
!
! $ Files
!
!     This routine opens the file named by INAME for read access, and
!     creates (destroying any pre-existing file if necessary) the
!     file named by ONAME.
!
! $ Particulars
!
!     This test routine allows existing test software that creates
!     DAFs using the high level writers to create native format
!     files and convert them to supported non-native formats formats
!     for testing purposes.
!
!     As new binary file formats for existing kernels are added to
!     the list of those officially supported, this routine and
!     the routines it calls may require significant updates.
!
! $ Examples
!
!     See the TSPICE routine T_DAFNN for an example of usage.
!
! $ Restrictions
!
!     This code is cobbled together in a rather non-robust way.
!     Do not attempt to convert binary kernels that are not 100%
!     correct in their structure and composition.  Using this
!     routine on files with questionable construction will yield
!     unpredictable results at best.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 11-FEB-2015 (NJB)
!
!        Updated to support DAS conversion.
!
!        Filled in header Exceptions section.
!
!-    SPICELIB Version 1.0.0, 28-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM

      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local Variables
!
      CHARACTER*(8)         ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(8)         IDWORD
      CHARACTER*(8)         TYPE


      INTEGER               HANDLE
      INTEGER               IAMH
      INTEGER               IARC
      INTEGER               IBFF
      INTEGER               IOSTAT
      INTEGER               UNIT

      LOGICAL               FOUND

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_BINGO' )

!
!     First things first, check to see if the handle manager has
!     an entry for ONAME.
!
      CALL ZZDDHFNH ( ONAME, HANDLE, FOUND )

      IF ( FOUND ) THEN

         CALL SETMSG ( 'Can not create file, ''#''.  It is currently ' &
         //            'loaded and associated with handle, #.  This ' &
         //            'routine may only create files not currently ' &
         //            'loaded.'                                       )
         CALL ERRCH  ( '#', ONAME                                      )
         CALL ERRINT ( '#', HANDLE                                     )
         CALL SIGERR ( 'SPICE(CANTCREATEFILE)'                         )
         CALL CHKOUT ( 'T_BINGO'                                       )
         RETURN

      END IF

!
!     Now, see if IFNAME is loaded in the handle manager.
!
      CALL ZZDDHFNH ( INAME, HANDLE, FOUND )

!
!     If it is loaded, then use the value of IARC to indicate the
!     file architecture.
!
      IF ( FOUND ) THEN

         CALL ZZDDHNFO ( HANDLE, FNAME, IARC, IBFF, IAMH, FOUND )

!
!     Otherwise, open the file and extract the 8 character IDWORD.
!
      ELSE

         CALL GETLUN ( UNIT )

         OPEN ( UNIT   = UNIT, &
                FILE   = INAME( 1 : RTRIM(INAME) ), &
                ACCESS = 'DIRECT', &
                RECL   = RECL, &
                STATUS = 'OLD', &
                IOSTAT = IOSTAT                     )

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to open file, ''#''. IOSTAT = #.' )
            CALL ERRCH  ( '#', INAME                                )
            CALL ERRINT ( '#', IOSTAT                               )
            CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                   )
            CALL CHKOUT ( 'T_BINGO'                                 )
            RETURN
         END IF

         READ ( UNIT   = UNIT, &
                REC    = 1, &
                IOSTAT = IOSTAT ) IDWORD

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to read ID word from file, ' &
            //            '''#''.  IOSTAT = #.'                )
            CALL ERRCH  ( '#', INAME                           )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'              )
            CALL CHKOUT ( 'T_BINGO'                            )
            RETURN
         END IF

         CALL IDW2AT ( IDWORD, ARCH, TYPE )

         IF ( EQSTR( ARCH, 'DAF' ) ) THEN
            IARC = DAF
         ELSE IF ( EQSTR( ARCH, 'DAS' ) ) THEN
            IARC = DAS
         ELSE
            IARC = 0
         END IF

         CLOSE ( UNIT = UNIT )

      END IF

!
!     IARC is set properly, branch appropriately.
!
      IF ( IARC .EQ. DAF ) THEN

         CALL T_BGODAF ( INAME, ONAME, OBFF )

      ELSE IF ( IARC .EQ. DAS ) THEN

         CALL T_BGODAS ( INAME, ONAME, OBFF )

      ELSE

         CALL SETMSG ( 'The architecture of file, ''#'', is ' &
         //            'unknown.  No conversion possible.'    )
         CALL SIGERR ( 'SPICE(UNKNOWNARCH)'                   )
         CALL CHKOUT ( 'T_BINGO'                              )
         RETURN

      END IF

      CALL CHKOUT ( 'T_BINGO' )
      RETURN

      END
