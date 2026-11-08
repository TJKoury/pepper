!$Procedure ZZDDHMNM ( Return unique enough DP number for a file )

      DOUBLE PRECISION FUNCTION ZZDDHMNM ( UNIT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return a unique enough DP number ("Magic NuMber") computed
!     using the contents of the file attached to the specified unit.
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
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               UNIT

      INTEGER               IDLEN
      PARAMETER           ( IDLEN  = 8 )

      INTEGER               NINTS
      PARAMETER           ( NINTS  = 20 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit attached to a file.
!
!     The function returns a unique enough DP number computed
!     using the contents of the file attached to the UNIT.
!
! $ Detailed_Input
!
!     UNIT        is the logical unit attached to a file opened for
!                 direct access prior to calling this routine.
!
! $ Detailed_Output
!
!     The function returns a DP number, computed using a few integers
!     read from the file attached to the UNIT, that is unique enough
!     for the handle manager to check if two files opened for READ
!     access are not the same file.
!
!     If reading the first record of the file or any of the lower level
!     routines called by this function fail for any reason, the
!     returned value is set to 0.D0.
!
! $ Parameters
!
!     IDLEN       is the length of the ID word.
!
!     NINTS       is the number of integers that will be read from
!                 a particular record of the file. NINTS must be
!                 big enough to make sure that unique pointers from
!                 the file record of DAF and DAS files are read and
!                 used to compute the output value.
!
!     The function also uses some DDH general parameters from
!     zzddhman.inc.
!
! $ Exceptions
!
!     Error free.
!
!     This routine and routines in its call tree signal several
!     SPICE(BUG) exceptions. They are signaled if the module or modules
!     in its calling tree are improperly configured to run on this
!     platform.
!
! $ Files
!
!     The input UNIT must be attached to a file opened for direct
!     access prior to calling this function.
!
! $ Particulars
!
!     This function reads the first IDLEN characters (assumed to be the
!     ID word) followed by NINTS integers from the first record of the
!     direct access file attached to the input UNIT.
!
!     If successful, it examines the ID word to determine the file
!     architecture.
!
!     For DAF files it then tries to determine the binary format.
!
!     For DAF files in the native binary format, it adds up NINTS
!     integers read from the first record to get initial output value.
!     Then it reads additional NINTS integers from the first descriptor
!     record and, if the second read is successful, it adds these
!     additional integers to the output value.
!
!     For DAF files in a non-native binary format supported by run-time
!     translation, it re-read NINTS integers from the first record as
!     characters, converts them to NINTS integers using ZZXLATEI, and
!     adds them up to get initial output value. Then it reads as
!     characters NINTS integers from the first descriptor record of the
!     file, converts them to NINTS integers using ZZXLATEI and ,if the
!     second read is successful, it adds these additional integers to
!     the output value.
!
!     For DAS files, text kernels and unrecognized files, it simply
!     adds up NINTS integers to get the output value.
!
!     If the initial read is not successful, the output value is set to
!     zero.
!
! $ Examples
!
!     See the caller routine, ZZDDHF2H.
!
! $ Restrictions
!
!     The input UNIT must be attached to a file opened for direct
!     access prior to calling this function.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.B. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 26-APR-2012 (BVS)
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters.
!
!     Character buffer size consistent with the number of integers
!     that will be read from the file.
!
      INTEGER               NCHARS
      PARAMETER           ( NCHARS = NINTS * 4 )

!
!     Minimum and maximum values for the range of ASCII printing
!     characters.
!
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )

      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )

!
!     Local variables.
!
      CHARACTER*(IDLEN)     ARCH
      CHARACTER*(IDLEN)     IDWORD
      CHARACTER*(IDLEN)     TYPE
      CHARACTER*(NCHARS)    STRBUF

      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )

      DOUBLE PRECISION      MNM

      INTEGER               BFF
      INTEGER               I
      INTEGER               INTARR ( NINTS )
      INTEGER               IOSTAT

      INTEGER               NATBFF
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               NUMSUP
      INTEGER               SUPIDX

      LOGICAL               FIRST

!
!     Saved variables.
!
      SAVE                  FIRST

      SAVE                  NATBFF
      SAVE                  SUPBFF
      SAVE                  NUMSUP

!
!     Data statements.
!
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /

!
!     Set default output value to zero.
!
      MNM      = 0.D0
      ZZDDHMNM = 0.D0

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHMNM' )
      END IF

!
!     Perform some initialization tasks.
!
      IF ( FIRST ) THEN

         CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, &
                         STRAMH, STRARC, STRBFF  )

!
!        Check FAILED() to handle the unlikely event that
!        ZZDDHINI signaled SPICE(BUG).
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZDDHMNM' )
            RETURN
         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

      END IF

!
!     Read ID word string followed by NINTS integers from the first
!     record of the file.
!
      READ ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) IDWORD, INTARR

      IF ( IOSTAT .EQ. 0 ) THEN

!
!        Read succeeded. Try to determine the file architecture and
!        type from the ID word. To do this, mimic the part of GETFAT
!        that deals only with the ID word. First replace any non
!        printing ASCII characters in the ID word with blanks, then use
!        IDW2AT on the "cleaned" ID word to get architecture and type.
!
         DO I = 1, IDLEN
            IF (      ( ICHAR(IDWORD(I:I)) .LT. MINPCH ) &
                 .OR. ( ICHAR(IDWORD(I:I)) .GT. MAXPCH ) ) THEN
               IDWORD(I:I) = ' '
            END IF
         END DO

         CALL IDW2AT ( IDWORD, ARCH, TYPE )

!
!        Compute the output value based on the file architecture.
!
         IF   ( ARCH .EQ. 'DAF' ) THEN

!
!           For DAF files, try to get the file's binary format.
!
            CALL ZZDDHPPF ( UNIT, DAF, BFF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZDDHMNM' )
               RETURN
            END IF

!
!           If the file is in a non-native format, we will need to read
!           the first record again, now directly as characters, and
!           translate these character to native integers.
!
            IF ( BFF .NE. NATBFF ) THEN

!
!              First, check if run-time translation is supported for
!              this BFF. This check, stolen from ZZDDHMAN, is needed
!              because ZZXLATEI accepts only BFFs for which translation
!              is guaranteed to be supported on this platform. If it
!              is not supported, simply get out (note that the default
!              return value was set to zero at the start.)
!
               SUPIDX = ISRCHI ( BFF, NUMSUP, SUPBFF )

               IF ( SUPIDX .EQ. 0 ) THEN
                  CALL CHKOUT ( 'ZZDDHMNM' )
                  RETURN
               END IF

!
!              Read the first record as characters and do translation.
!
               READ ( UNIT=UNIT, REC=1, IOSTAT=IOSTAT ) IDWORD, STRBUF

               CALL ZZXLATEI ( BFF, STRBUF, NINTS, INTARR )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZDDHMNM' )
                  RETURN
               END IF

            END IF

!
!           Add integers from the file record to the output value.
!
            DO I = 1, NINTS
               MNM = MNM + INTARR(I)
            END DO

!
!           Read more integers from the start of the first descriptor
!           record without regard to the file's binary format and,
!           if successful, add them to the total.
!
            READ ( UNIT=UNIT, REC=INTARR(18), IOSTAT=IOSTAT ) INTARR

            IF ( IOSTAT .EQ. 0 ) THEN

               DO I = 1, NINTS
                  MNM = MNM + INTARR(I)
               END DO

            END IF

         ELSE IF ( ARCH .EQ. 'DAS' ) THEN

!
!           For DAS files, for now, add up integers from the first
!           record to get the output value.
!
            DO I = 1, NINTS
               MNM = MNM + INTARR(I)
            END DO

         ELSE

!
!           For all other files, add up integers from the first record
!           to get the output value.
!
            DO I = 1, NINTS
               MNM = MNM + INTARR(I)
            END DO

         END IF

      ELSE

!
!        The read of the file record failed. Do nothing as the output
!        value has already been set at the start of the function.
!

      END IF

      ZZDDHMNM = MNM

      CALL CHKOUT ( 'ZZDDHMNM' )

      RETURN
      END
