!$Procedure T_WCLK01 ( Write a type 1 SCLK kernel )

      SUBROUTINE T_WCLK01 ( FNAME,  COMS,   CLKID,  KERID, TIMSYS, &
                            NFIELD, MODULI, OFFSET, DELIM, NPARTS, &
                            PSTART, PSTOP,  NCOEFF, COEFFS        )

! $ Abstract
!
!     Write a type 1 SCLK kernel whose kernel variable assignments are
!     based on data passed in by the caller.
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
!     SCLK
!
! $ Keywords
!
!     SCLK
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'sclk.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         FNAME
      CHARACTER*(*)         COMS  ( LBCELL : * )
      INTEGER               CLKID
      CHARACTER*(*)         KERID
      CHARACTER*(*)         TIMSYS
      INTEGER               NFIELD
      DOUBLE PRECISION      MODULI ( NFIELD )
      DOUBLE PRECISION      OFFSET ( NFIELD )
      CHARACTER*(*)         DELIM
      INTEGER               NPARTS
      DOUBLE PRECISION      PSTART ( NPARTS )
      DOUBLE PRECISION      PSTOP  ( NPARTS )
      INTEGER               NCOEFF
      DOUBLE PRECISION      COEFFS ( 3, NCOEFF )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   SCLK kernel name.
!     COMS       I   Kernel comment block.
!     CLKID      I   SCLK ID code.
!     KERID      I   Kernel ID: date in text kernel format.
!     TIMSYS     I   Time system name.
!     NFIELD     I   Number of fields in SCLK string format.
!     MODULI     I   Moduli of SCLK string format.
!     OFFSET     I   Offsets of SCLK string format.
!     DELIM      I   SCLK string output delimiter.
!     NPARTS     I   Number of partitions.
!     PSTART     I   Partition start times.
!     PSTOP      I   Partition stop times.
!     NCOEFF     I   Number of SCLK coefficient records.
!     COEFFS     I   SCLK coefficient records.
!
! $ Detailed_Input
!
!     FNAME      is the name of the output SCLK kernel to create.
!                This kernel must not already exist.
!
!     COMS       is a SPICELIB character cell containing a block
!                of comments to be inserted in the SCLK kernel
!                after the ID word and before the first "begindata"
!                marker.
!
!     CLKID      is the integer ID code of the clock with which
!                the output kernel is associated. This ID normally
!                is the same as the integer ID of the spacecraft
!                on which the clock is mounted.
!
!     KERID      is the SCLK Kernel ID string. This is a
!                date in text kernel format: The date starts
!                with the character '@' and contains no embedded
!                blanks. Example:
!
!                   @01-JAN-2000/12:00:00
!
!     TIMSYS     is the name of the parallel time system referenced
!                by the clock correlation coefficients. Supported
!                values are
!
!                   'TDB'
!                   'TDT'
!
!                case and blanks are not significant in TIMSYS.
!
!     NFIELD     is the number of fields in SCLK string format.
!
!     MODULI     is an array containing the moduli of SCLK string
!                format. The order of the moduli in the array
!                corresponds to left-to-right order in the SCLK string
!                format.
!
!     OFFSET     is an array containing the offsets of SCLK string
!                format. The order of the offsets in the array
!                corresponds to left-to-right order in the SCLK string
!                format.
!
!     DELIM      is the SCLK string output delimiter. This delimiter
!                is used by the SCLK system to create SCLK strings.
!                The delimiter must belong to the set contained in
!                the parameter DELIMS declared in the INCLUDE file
!                sclk.inc.
!
!     NPARTS     is the number of partitions for the SCLK data set.
!                NPARTS is used to define the dimensions of the
!                input arrays PSTART and PSTOP.
!
!     PSTART     is an array of partition start times. PSTART has
!                dimension NPARTS.
!
!     PSTOP      is an array of partition stop times.  PSTOP has
!                dimension NPARTS.
!
!     NCOEFF     is the number of SCLK coefficient records in the
!                array COEFFS.
!
!     COEFFS     is a 3 x NCOEFF array of SCLK coefficient records.
!                The elements having indices
!
!                   (1,I), (2,I), (3,I)
!
!                are, respectively, the Ith tick coefficient, the Ith
!                parallel time system coefficient, and the Ith clock
!                rate coefficient.
!
! $ Detailed_Output
!
!     None. See the section Files below.
!
! $ Parameters
!
!     See the INCLUDE file sclk.inc.
!
! $ Exceptions
!
!     1)  If the file designated by FNAME exists, the error
!         SPICE(FILEEXISTS) is signaled.
!
!     2)  If the SCLK kernel ID string does not contain
!         a date in SPICE text kernel format, the error
!         SPICE(INVALIDIDSTRING) is signaled.
!
!     3)  If the input time system name is not equivalent
!         to (ignoring case and blanks) a supported value,
!         the error SPICE(INVALIDTIMESYSTEM) is signaled.
!
!     4)  If the SCLK string field count is out of range,
!         the error SPICE(INVALIDFIELDCOUNT) is signaled.
!
!     5)  If any SCLK field modulus is out of range,
!         the error SPICE(INVALIDMODULUS) is signaled.
!
!     6)  If the product of the SCLK field moduli is out of range,
!         the error SPICE(INVALIDMODULI) is signaled.
!
!     7)  If any SCLK field offset is out of range,
!         the error SPICE(INVALIDOFFSET) is signaled.
!
!     8)  If the output SCLK string field delimiter is not
!         a supported value, the error SPICE(INVALIDDELIMITER)
!         is signaled.
!
!     9)  If the partition count is out of range,
!         the error SPICE(BADPARTITIONCOUNT) is signaled.
!
!     10) If any partition start time is out of range,
!         the error SPICE(BADPARTITIONSTART) is signaled.
!
!     11) If any partition start time is non-integral,
!         the error SPICE(BADPARTITIONSTART) is signaled.
!
!     12) If any partition start time cannot be converted to a
!         string via a Fortran WRITE, the error
!         SPICE(WRITEERROR) is signaled.
!
!     13) If any partition stop time is out of range,
!         the error SPICE(BADPARTITIONSTOP) is signaled.
!
!     14) If any partition stop time is non-integral,
!         the error SPICE(BADPARTITIONSTOP) is signaled.
!
!     15) If any partition stop time cannot be converted to a
!         string via a Fortran WRITE, the error
!         SPICE(WRITEERROR) is signaled.
!
!     16) If the SCLK coefficient record count is out of range,
!         the error SPICE(BADRECORDCOUNT) is signaled.
!
!     17) If the first tick coefficient is negative, the
!         error SPICE(NEGATIVETICK) is signaled.
!
!     18) If the tick coefficients are not in strictly increasing
!         order, the error SPICE(TICKSOUTOFORDER) is signaled.
!
!     19) If the parallel time system coefficients are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     20) If any SCLK rate value is non-positive, the error
!         SPICE(NONPOSITIVERATE) is signaled.
!
!     21) If 3-element coefficient record cannot be converted to a
!         string via a Fortran WRITE, the error
!         SPICE(WRITEERROR) is signaled.
!
! $ Files
!
!     The output of this routine is the SCLK kernel named
!     FNAME having contents provided by the input arguments
!     to this routine.
!
!     No leapseconds kernel need be loaded in order to use this
!     routine.
!
! $ Particulars
!
!     This routine is intended to support creation of SCLK kernels
!     for testing. The routine could be used as a component of
!     a general purpose SCLK kernel writing routine.
!
! $ Examples
!
!     1) The following program creates the SCLK kernel shown
!        at the bottom of this header section:
!
!
!        PROGRAM TWRT
!        IMPLICIT NONE
!  C
!  C     SPICELIB Functions
!  C
!        LOGICAL               EXISTS
!  C
!  C     Local parameters
!  C
!        CHARACTER*(*)         FNAME
!        PARAMETER           ( FNAME  = 'wrt.tsc' )
!
!        INTEGER               LBCELL
!        PARAMETER           ( LBCELL = -5 )
!
!        INTEGER               NCOM
!        PARAMETER           ( NCOM   = 3 )
!
!        CHARACTER*(*)         DELIM
!        PARAMETER           ( DELIM  = ':' )
!
!        INTEGER               NFIELD
!        PARAMETER           ( NFIELD = 4 )
!
!        INTEGER               NPARTS
!        PARAMETER           ( NPARTS = 3 )
!
!        INTEGER               NCOEFF
!        PARAMETER           ( NCOEFF = 10 )
!
!        INTEGER               LNSIZE
!        PARAMETER           ( LNSIZE = 80 )
!
!  C
!  C     Local variables
!  C
!        CHARACTER*(LNSIZE)    COMS ( LBCELL : NCOM )
!        CHARACTER*(LNSIZE)    LINE
!
!        DOUBLE PRECISION      COEFFS ( 3, NCOEFF )
!        DOUBLE PRECISION      DELTA
!        DOUBLE PRECISION      MODULI ( NFIELD )
!        DOUBLE PRECISION      OFFSET ( NFIELD )
!        DOUBLE PRECISION      PSTART ( NPARTS )
!        DOUBLE PRECISION      PSTOP  ( NPARTS )
!
!        INTEGER               I
!
!
!        IF ( EXISTS(FNAME) ) THEN
!           CALL DELFIL ( FNAME )
!        END IF
!
!        CALL SSIZEC ( NCOM, COMS )
!
!        DO I = 1, NCOM
!
!           LINE = '>>>>>>>> Comment line # <<<<<<<<<<<<'
!           CALL REPMI ( LINE, '#', I, LINE )
!
!           CALL APPNDC ( LINE, COMS )
!
!        END DO
!
!        MODULI(1) = 1000000
!        MODULI(2) =      10
!        MODULI(3) =       9
!        MODULI(4) =       8
!
!        OFFSET(1) =       1
!        OFFSET(2) =       2
!        OFFSET(3) =       3
!        OFFSET(4) =       4
!
!        DO I = 1, NPARTS
!           PSTART(I) = I
!           PSTOP (I) = 720000 + I
!        END DO
!
!        DELTA = 1.D-9
!
!        DO I = 1, NCOEFF
!
!           COEFFS(1,I) = (I-1) * 180000.D0
!
!           COEFFS(2,I) = (I-1) *  90000.D0
!
!           COEFFS(3,I) = 2.D0  +  I*DELTA
!
!        END DO
!
!        CALL T_WCLK01 ( FNAME,
!       .                COMS,
!       .                -99,
!       .                '@01-JAN-2000/12:00:00',
!       .                'TDB',
!       .                NFIELD,
!       .                MODULI,
!       .                OFFSET,
!       .                DELIM,
!       .                NPARTS,
!       .                PSTART,
!       .                PSTOP,
!       .                NCOEFF,
!       .                COEFFS       )
!
!        END
!
!
!     The SCLK kernel shown below has been modified slightly
!     to fit within the 72-column limit imposed by standard
!     Fortran 77 format restrictions: the SCLK coefficients
!     assignments have been shifted left.
!
!
!     KPL/SCLK
!
!     >>>>>>>> Comment line 1 <<<<<<<<<<<<
!     >>>>>>>> Comment line 2 <<<<<<<<<<<<
!     >>>>>>>> Comment line 3 <<<<<<<<<<<<
!
!     \begindata
!
!        SCLK_KERNEL_ID                  = ( @01-JAN-2000/12:00:00 )
!
!        SCLK_DATA_TYPE_99               = ( 1 )
!
!        SCLK01_TIME_SYSTEM_99           = ( 1 )
!        SCLK01_N_FIELDS_99              = ( 4 )
!        SCLK01_MODULI_99                = ( 1000000 10 9 8 )
!        SCLK01_OFFSETS_99               = ( 4 3 2 1 )
!        SCLK01_OUTPUT_DELIM_99          = ( 2 )
!
!        SCLK_PARTITION_START_99         = ( 1.0,
!                                            2.0,
!                                            3.0 )
!
!        SCLK_PARTITION_END_99           = ( 720001.0,
!                                            720002.0,
!                                            720003.0 )
!
!        SCLK01_COEFFICIENTS_99          = (
!
!                0.0    0.0000000000000000E+00    0.2000000001000000E+01
!           180000.0    0.9000000000000000E+05    0.2000000002000000E+01
!           360000.0    0.1800000000000000E+06    0.2000000003000000E+01
!           540000.0    0.2700000000000000E+06    0.2000000004000000E+01
!           720000.0    0.3600000000000000E+06    0.2000000005000000E+01
!           900000.0    0.4500000000000000E+06    0.2000000006000000E+01
!          1080000.0    0.5400000000000000E+06    0.2000000007000000E+01
!          1260000.0    0.6300000000000000E+06    0.2000000008000000E+01
!          1440000.0    0.7200000000000000E+06    0.2000000009000000E+01
!          1620000.0    0.8100000000000000E+06    0.2000000010000000E+01
!        )
!
!     \begintext
!
!
!
! $ Restrictions
!
!     1) This routine supports only type 1 spacecraft clocks.
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
!-    TSPICE Version 1.1.0, 31-JAN-2014 (BVS)
!
!        BUG FIX: changed (MODULI(I) .LT. 2) to (MODULI(I) .LT. 1.D0)
!        as comparison of a DP must be done to a DP value and the
!        smallest allowed moduli value is 1.
!
!-    TSPICE Version 1.0.0, 21-FEB-2008 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDC
      INTEGER               LASTNB

      LOGICAL               EQSTR
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Test utility functions
!
      EXTERNAL              BEGDAT
      CHARACTER*(80)        BEGDAT

      EXTERNAL              BEGTXT
      CHARACTER*(80)        BEGTXT

!
!     Local Parameters
!
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 200 )

!
!     Local Variables
!
!
!     We use a longer, non-standard length for ABCORR because we
!     want to include embedded blanks for testing.
!
      CHARACTER*(NDELIM)    DLCHRS
      CHARACTER*(MSGLEN)    ERRMSG
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(LNGSIZ)    LONGLN
      CHARACTER*(LNSIZE)    NUMSTR

      DOUBLE PRECISION      SP2000
      DOUBLE PRECISION      PRODM

      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               L
      INTEGER               LNB
      INTEGER               START
      INTEGER               STRID
      INTEGER               TIMCDE
      INTEGER               UNIT

!
!     Saved variables
!
      SAVE                  DLCHRS

!
!     Initial values
!
      DATA                  DLCHRS / DELIMS /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_WCLK01' )

!
!     The clock ID to use in strings is the negative of
!     the input ID.
!
      STRID = -CLKID

!
!     Don't overwrite an existing file.
!
      IF ( EXISTS(FNAME) ) THEN

         CALL SETMSG ( 'File # already exists.' )
         CALL ERRCH  ( '#', FNAME               )
         CALL SIGERR ( 'SPICE(FILEEXISTS)'      )
         CALL CHKOUT ( 'T_WCLK01'               )
         RETURN

      END IF

!
!     Open the SCLK kernel as a new text file.
!
      CALL TXTOPN ( FNAME, UNIT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_WCLK01' )
         RETURN
      END IF

!
!     Write the SCLK ID word.
!
      CALL WRITLN ( ' ',         UNIT )
      CALL WRITLN ( 'KPL/SCLK',  UNIT )
      CALL WRITLN ( ' ',         UNIT )

!
!     Write any initial comments that may have been
!     supplied.
!
      DO I = 1, CARDC(COMS)
         CALL WRITLN ( COMS(I), UNIT )
      END DO

!
!     Write the "begindata" marker.
!
      CALL WRITLN ( ' ',       UNIT )
      CALL WRITLN ( BEGDAT(),  UNIT )
      CALL WRITLN ( ' ',       UNIT )

!
!     Write the kernel ID. The kernel ID must be a date
!     of the form
!
!        @<date>
!
!     where <date contains no embedded blanks.
!
      CALL LJUST ( KERID, LINE )
      L = LASTNB(LINE)

      IF ( L .EQ. 0 ) THEN

         CALL SETMSG ( 'Kernel ID written to file # must be ' &
         //            'non-blank.' )
         CALL ERRCH  ( '#', FNAME                )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'  )
         CALL CHKOUT ( 'T_WCLK01'                )
         RETURN

      END IF

      IF (  INDEX( LINE(:L), ' ' )  .NE.  0  )  THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must ' &
         //            'contain no embedded blanks'           )
         CALL ERRCH  ( '#', KERID                             )
         CALL ERRCH  ( '#', FNAME                             )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'               )
         CALL CHKOUT ( 'T_WCLK01'                             )
         RETURN

      END IF


      IF ( LINE(1:1) .NE. '@' ) THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must ' &
         //            'be of the form @<date> where date ' &
         //            'contains no embedded blanks.'       )
         CALL ERRCH  ( '#', KERID                           )
         CALL ERRCH  ( '#', FNAME                           )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'             )
         CALL CHKOUT ( 'T_WCLK01'                           )
         RETURN

      END IF

      IF ( L .LT. 9 ) THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must ' &
         //            'be of the form @<date> where date ' &
         //            'contains no embedded blanks.'       )
         CALL ERRCH  ( '#', KERID                           )
         CALL ERRCH  ( '#', FNAME                           )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'             )
         CALL CHKOUT ( 'T_WCLK01'                           )
         RETURN

      END IF

      CALL TPARSE ( LINE(2:L), SP2000, ERRMSG )

      IF ( ERRMSG .NE. ' ' ) THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must ' &
         //            'be of the form @<date> where date ' &
         //            'contains no embedded blanks. TPARSE ' &
         //            'returned message < # > '            )
         CALL ERRCH  ( '#', KERID                           )
         CALL ERRCH  ( '#', FNAME                           )
         CALL ERRCH  ( '#', ERRMSG                          )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'             )
         CALL CHKOUT ( 'T_WCLK01'                           )
         RETURN

      END IF


      LINE = '   SCLK_KERNEL_ID                  = ( # )'
      CALL REPMC ( LINE, '#', KERID, LINE )

      CALL WRITLN ( LINE, UNIT )
      CALL WRITLN ( ' ',  UNIT )

!
!     Write the SCLK data type: for this routine, the value
!     is always 1.
!
      LINE = '   SCLK_DATA_TYPE_#              = ( 1 )'
      CALL REPMI ( LINE, '#', STRID, LINE )

      CALL WRITLN ( LINE, UNIT )
      CALL WRITLN ( ' ',  UNIT )

!
!     Write the time system code.
!
      IF (  EQSTR( TIMSYS, 'TDB')  ) THEN

         TIMCDE = 1

      ELSE IF (  EQSTR( TIMSYS, 'TDT')  ) THEN

         TIMCDE = 2

      ELSE

         CALL SETMSG ( 'SCLK time system for file # must be ' &
         //            'TDB or TDT but was #.'               )
         CALL ERRCH  ( '#', FNAME                            )
         CALL ERRCH  ( '#', TIMSYS                           )
         CALL SIGERR ( 'SPICE(INVALIDTIMESYSTEM)'            )
         CALL CHKOUT ( 'T_WCLK01'                            )
         RETURN

      END IF

      LINE = '   SCLK_DATA_TYPE_#              = ( 1 )'
      LINE = '   SCLK01_TIME_SYSTEM_#          = ( # )'
      CALL REPMI ( LINE, '#', STRID,  LINE )
      CALL REPMI ( LINE, '#', TIMCDE, LINE )

!
!     No blank line follows this output because the clock
!     attribute parameters are written as a contiguous
!     block. Why?  ...tradition.
!
      CALL WRITLN ( LINE, UNIT )

!
!     Write the number of fields.
!
      IF ( ( NFIELD .LT. 1 ) .OR. ( NFIELD .GT. MXNFLD )  ) THEN

         CALL SETMSG ( 'SCLK field count for file # must be ' &
         //            'in range 1:# but was #.'             )
         CALL ERRCH  ( '#', FNAME                            )
         CALL ERRINT ( '#', MXNFLD                           )
         CALL ERRINT ( '#', NFIELD                           )
         CALL SIGERR ( 'SPICE(INVALIDFIELDCOUNT)'            )
         CALL CHKOUT ( 'T_WCLK01'                            )
         RETURN

      END IF

      LINE = '   SCLK01_N_FIELDS_#             = ( # )'
      CALL REPMI ( LINE, '#', STRID,  LINE )
      CALL REPMI ( LINE, '#', NFIELD, LINE )

      CALL WRITLN ( LINE, UNIT )

!
!     Write the moduli. Recall these are d.p. numbers.
!
      DO I = 1, NFIELD
!
!        All moduli must be in the range 1 : 1e14.
!
         IF (      ( MODULI(I) .LT. 1.D0  ) &
              .OR. ( MODULI(I) .GT. 1.D14 )  ) THEN

            CALL SETMSG ( 'SCLK modulus # for file # must be ' &
            //            'in range 1 : # but was #.'           )
            CALL ERRINT ( '#', I                                )
            CALL ERRCH  ( '#', FNAME                            )
            CALL ERRDP  ( '#', 1.D14                            )
            CALL ERRDP  ( '#', MODULI(I)                        )
            CALL SIGERR ( 'SPICE(INVALIDMODULUS)'               )
            CALL CHKOUT ( 'T_WCLK01'                            )
            RETURN

         END IF

      END DO

!
!     Make sure the product of the moduli can fit in the d.p. mantissa
!     without truncation.
!
      PRODM = MODULI(1)

      DO I = 2, NFIELD
         PRODM = PRODM * MODULI(I)
      END DO

      IF ( PRODM .GT. 1.D15 ) THEN

         CALL SETMSG ( 'SCLK modulus product for file # must be ' &
         //            'in range # : # but was #. (Lower bound ' &
         //            'is 2**NFIELD.)'                         )
         CALL ERRCH  ( '#', FNAME                               )
         CALL ERRINT ( '#', 2**NFIELD                           )
         CALL ERRDP  ( '#', 1.D15                               )
         CALL ERRDP  ( '#', PRODM                               )
         CALL SIGERR ( 'SPICE(INVALIDMODULI)'                   )
         CALL CHKOUT ( 'T_WCLK01'                               )
         RETURN

      END IF

!
!     If we have 4 or fewer fields, write the moduli out
!     on one line on the right hand side. Otherwise
!     stack up the values on multiple lines.
!
!     Build the line in a long string; check the length
!     before writing the line.
!

      LONGLN = '   SCLK01_MODULI_#               = ( #'
      CALL REPMI ( LONGLN, '#', STRID,  LONGLN )

      IF ( NFIELD .LE. 4 ) THEN

         DO I = 1, NFIELD-1

            CALL REPMC ( LONGLN, '#', '# #', LONGLN )

            CALL DPFMT  ( MODULI(I), 'XXXXXXXXXXXXXX', NUMSTR )
            CALL LJUST  ( NUMSTR,                      NUMSTR )
            CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         END DO

         CALL DPFMT  ( MODULI(NFIELD), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                           NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         CALL SUFFIX ( ')', 1, LONGLN )

         LNB = LASTNB(LONGLN )

         IF ( LNB .LE. 80 ) THEN

            CALL WRITLN ( LONGLN, UNIT )

         END IF

      END IF

!
!     The following case is for moduli assignments that
!     span multiple lines.
!
      IF (  ( NFIELD .GT. 4 ) .OR. ( LNB .GT. 80 )  ) THEN
!
!        Save the start position for the moduli.
!
         START = LASTNB(LONGLN)
!
!
!        Fill in the first line and write it.
!
         CALL DPFMT  ( MODULI(1), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                      NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

!
!        Append a comma to the line.
!
         CALL SUFFIX ( ',', 0, LONGLN )

         CALL WRITLN ( LONGLN, UNIT )

!
!        Fill in the lines preceding the last line and
!        write these as well.
!
         DO I = 2, NFIELD-1

            LONGLN         = ' '
            LONGLN(START:) = '#,'

            CALL DPFMT  ( MODULI(I), 'XXXXXXXXXXXXXX', NUMSTR )
            CALL LJUST  ( NUMSTR,                      NUMSTR )
            CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

            CALL WRITLN ( LONGLN, UNIT )

         END DO

!
!        Fill in and write the last line.
!
         LONGLN         = ' '
         LONGLN(START:) = '# )'

         CALL DPFMT  ( MODULI(NFIELD), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                           NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         CALL WRITLN ( LONGLN, UNIT )

      END IF


!
!     Write the offsets. Recall these are d.p. numbers.
!     The method is the same as for the moduli, except for
!     the range check.
!
      DO I = 1, NFIELD
!
!        All offsets must be in the range 0 : moduli(i)-1.
!
         IF (      ( OFFSET(I) .LT. 0.D0        ) &
              .OR. ( OFFSET(I) .GT. MODULI(1)-1 )  ) THEN

            CALL SETMSG ( 'SCLK offset # for file # must be ' &
            //            'in range 0 : # but was #.'           )
            CALL ERRINT ( '#', I                                )
            CALL ERRCH  ( '#', FNAME                            )
            CALL ERRDP  ( '#', MODULI(I) - 1                    )
            CALL ERRDP  ( '#', OFFSET(I)                        )
            CALL SIGERR ( 'SPICE(INVALIDOFFSET)'                )
            CALL CHKOUT ( 'T_WCLK01'                            )
            RETURN

         END IF

      END DO

!
!     If we have 4 or fewer fields, write the offsets out
!     on one line on the right hand side. Otherwise
!     stack up the values on multiple lines.
!
!     Build the line in a long string; check the length
!     before writing the line.
!
      LONGLN = '   SCLK01_OFFSETS_#              = ( #'
      CALL REPMI ( LONGLN, '#', STRID,  LONGLN )

      IF ( NFIELD .LE. 4 ) THEN

         DO I = 1, NFIELD-1

            CALL REPMC ( LONGLN, '#', '# #', LONGLN )

            CALL DPFMT  ( OFFSET(I), 'XXXXXXXXXXXXXX', NUMSTR )
            CALL LJUST  ( NUMSTR,                      NUMSTR )
            CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         END DO

         CALL DPFMT  ( OFFSET(NFIELD), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                            NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         CALL SUFFIX ( ')', 1, LONGLN )

         LNB = LASTNB(LONGLN )

         IF ( LNB .LE. 80 ) THEN

            CALL WRITLN ( LONGLN, UNIT )

         END IF

      END IF

!
!     The following case is for offset assignments that
!     span multiple lines.
!
      IF (  ( NFIELD .GT. 4 ) .OR. ( LNB .GT. 80 )  ) THEN
!
!        Save the start position for the offsets.
!
         START = LASTNB(LONGLN)
!
!
!        Fill in the first line and write it.
!
         CALL DPFMT  ( OFFSET(1), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                      NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

!
!        Append a comma to the line.
!
         CALL SUFFIX ( ',', 0, LONGLN )

         CALL WRITLN ( LONGLN, UNIT )

!
!        Fill in the lines preceding the last line and
!        write these as well.
!
         DO I = 2, NFIELD-1

            LONGLN         = ' '
            LONGLN(START:) = '#,'

            CALL DPFMT  ( OFFSET(I), 'XXXXXXXXXXXXXX', NUMSTR )
            CALL LJUST  ( NUMSTR,                      NUMSTR )
            CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

            CALL WRITLN ( LONGLN, UNIT )

         END DO

!
!        Fill in and write the last line.
!
         LONGLN         = ' '
         LONGLN(START:) = '# )'

         CALL DPFMT  ( OFFSET(NFIELD), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                           NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         CALL WRITLN ( LONGLN, UNIT )

      END IF

!
!     Write the output delimiter line.
!
      J = INDEX ( DLCHRS, DELIM )

      IF ( J .EQ. 0 ) THEN

         CALL SETMSG ( 'SCLK output delimiter for file # must be ' &
         //            'in set {#} but was #.'                  )
         CALL ERRCH  ( '#', FNAME                               )

         DO I = 1, NDELIM-1
            CALL ERRCH ( '#', '#'//DLCHRS(I:I)//',#'            )
         END DO

         CALL ERRCH  ( '#', DLCHRS(NDELIM:NDELIM)               )
         CALL ERRCH  ( '#', DELIM                               )
         CALL SIGERR ( 'SPICE(INVALIDDELIMITER)'                )
         CALL CHKOUT ( 'T_WCLK01'                               )
         RETURN

      END IF

      LINE   = '   SCLK01_OUTPUT_DELIM_#         = ( # )'
      CALL REPMI ( LINE, '#', STRID, LINE )
      CALL REPMI ( LINE, '#', J,     LINE )

      CALL WRITLN ( LINE, UNIT )

!
!     A blank line separates the clock attributes from the
!     partition data.
!
      CALL WRITLN ( ' ', UNIT )

!
!     Write the partition start data; one value only per line.
!
      IF (  ( NPARTS .LT. 1 ) .OR. ( NPARTS .GT. MXPART ) ) THEN

         CALL SETMSG ( 'SCLK partition count for file # must be ' &
         //            'in range 1 : # but was #.'                 )
         CALL ERRCH  ( '#', FNAME                                  )
         CALL ERRINT ( '#', MXPART                                 )
         CALL ERRINT ( '#', NPARTS                                 )
         CALL SIGERR ( 'SPICE(BADPARTITIONCOUNT)'                  )
         CALL CHKOUT ( 'T_WCLK01'                                  )
         RETURN

      END IF

      DO I = 1, NPARTS
!
!        Partition start readouts must be at least zero
!        and must be strictly less than the corresponding
!        stop readouts.
!
         IF (      ( PSTART(I) .LT. 0.D0     ) &
              .OR. ( PSTART(I) .GE. PSTOP(I) )  ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # must be ' &
            //            'in range 0 : # but was #.'                 )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTOP(I)-1                             )
            CALL ERRDP  ( '#', PSTART(I)                              )
            CALL SIGERR ( 'SPICE(BADPARTITIONSTART)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

         END IF

!
!        Partition start times must be integral.
!
         IF (  PSTART(I)  .NE.  DNINT( PSTART(I) )  ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # must be ' &
            //            'integral but was #.'                       )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTART(I)                              )
            CALL SIGERR ( 'SPICE(BADPARTITIONSTART)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

         END IF

      END DO


      LINE   = '   SCLK_PARTITION_START_#        = ( #'
      CALL REPMI ( LINE, '#', STRID, LINE )

!
!     Record the value start index in the line.
!
      START = LASTNB(LINE)

!
!     We must format partition bounds using a Fortran internal write
!     because the SPICE formatting routines can't handle 16
!     significant digits.
!
      WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTART(1)

      IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # could ' &
            //            'not be formatted; value = #; IOSTAT = #.'  )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTART(1)                              )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL SIGERR ( 'SPICE(WRITEERROR)'                         )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

      END IF

      CALL REPMC  ( LINE, '#', NUMSTR, LINE )

      IF ( NPARTS .GT. 1 ) THEN

         CALL SUFFIX ( ',', 0, LINE )
      ELSE
         CALL SUFFIX ( ')', 1, LINE )
      END IF

      CALL WRITLN ( LINE, UNIT )


      IF ( NPARTS .GT. 1 ) THEN
!
!        Write the rest of the partitions.
!
         DO I = 2, NPARTS-1

            WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTART(I)

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'SCLK partition start # for file # could ' &
               //            'not be formatted; value = #; IOSTAT = #.')
               CALL ERRINT ( '#', I                                    )
               CALL ERRCH  ( '#', FNAME                                )
               CALL ERRDP  ( '#', PSTART(I)                            )
               CALL ERRINT ( '#', IOSTAT                               )
               CALL SIGERR ( 'SPICE(WRITEERROR)'                       )
               CALL CHKOUT ( 'T_WCLK01'                                )
               RETURN

            END IF

            CALL LJUST ( NUMSTR, NUMSTR )

            LINE         = ' '
            LINE(START:) = NUMSTR

            CALL SUFFIX ( ',', 0, LINE )

            CALL WRITLN ( LINE, UNIT )

         END DO


         WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTART(NPARTS)

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # could ' &
            //            'not be formatted; value = #; IOSTAT = #.')
            CALL ERRINT ( '#', I                                    )
            CALL ERRCH  ( '#', FNAME                                )
            CALL ERRDP  ( '#', PSTART(I)                            )
            CALL ERRINT ( '#', IOSTAT                               )
            CALL SIGERR ( 'SPICE(WRITEERROR)'                       )
            CALL CHKOUT ( 'T_WCLK01'                                )
            RETURN

         END IF

         CALL LJUST ( NUMSTR, NUMSTR )

         LINE         = ' '
         LINE(START:) = NUMSTR

         CALL SUFFIX ( ')', 1, LINE )

         CALL WRITLN ( LINE, UNIT )

      END IF

      CALL WRITLN ( ' ', UNIT )


!
!     Write the partition stop data; one value only per line.
!
      DO I = 1, NPARTS
!
!        Partition stop readouts must be strictly greater than the
!        partition start values (this has been checked) and must not
!        exceed the product of the moduli.
!
         IF (  PSTOP(I) .GT. PRODM  ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # must be ' &
            //            'in range # : # but was #.'                 )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTART(I)                              )
            CALL ERRDP  ( '#', PRODM                                  )
            CALL ERRDP  ( '#', PSTOP(I)                               )
            CALL SIGERR ( 'SPICE(BADPARTITIONSTOP)'                   )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

         END IF

!
!        Partition stop times must be integral.
!
         IF (  PSTOP(I)  .NE.  DNINT( PSTOP(I) )  ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # must be ' &
            //            'integral but was #.'                       )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTOP(I)                               )
            CALL SIGERR ( 'SPICE(BADPARTITIONSTOP)'                   )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

         END IF

      END DO


      LINE   = '   SCLK_PARTITION_END_#          = ( #'
      CALL REPMI ( LINE, '#', STRID, LINE )

!
!     Record the value start index in the line.
!
      START = LASTNB(LINE)

!
!     We must format partition bounds using a Fortran internal write
!     because the SPICE formatting routines can't handle 16
!     significant digits.
!
      WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTOP(1)

      IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # could ' &
            //            'not be formatted; value = #; IOSTAT = #.'  )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTOP(1)                               )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL SIGERR ( 'SPICE(WRITEERROR)'                   )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

      END IF

      CALL REPMC  ( LINE, '#', NUMSTR, LINE )

      IF ( NPARTS .GT. 1 ) THEN

         CALL SUFFIX ( ',', 0, LINE )
      ELSE
         CALL SUFFIX ( ')', 1, LINE )
      END IF

      CALL WRITLN ( LINE, UNIT )


      IF ( NPARTS .GT. 1 ) THEN
!
!        Write the rest of the partitions.
!
         DO I = 2, NPARTS-1

            WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTOP(I)

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'SCLK partition stop # for file # could ' &
               //            'not be formatted; value = #; IOSTAT = #.')
               CALL ERRINT ( '#', I                                    )
               CALL ERRCH  ( '#', FNAME                                )
               CALL ERRDP  ( '#', PSTOP(I)                             )
               CALL ERRINT ( '#', IOSTAT                               )
               CALL SIGERR ( 'SPICE(WRITEERROR)'                 )
               CALL CHKOUT ( 'T_WCLK01'                                )
               RETURN

            END IF

            CALL LJUST ( NUMSTR, NUMSTR )

            LINE         = ' '
            LINE(START:) = NUMSTR

            CALL SUFFIX ( ',', 0, LINE )

            CALL WRITLN ( LINE, UNIT )

         END DO


         WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTOP(NPARTS)

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # could ' &
            //            'not be formatted; value = #; IOSTAT = #.')
            CALL ERRINT ( '#', I                                    )
            CALL ERRCH  ( '#', FNAME                                )
            CALL ERRDP  ( '#', PSTOP(I)                             )
            CALL ERRINT ( '#', IOSTAT                               )
            CALL SIGERR ( 'SPICE(WRITEERROR)'                 )
            CALL CHKOUT ( 'T_WCLK01'                                )
            RETURN

         END IF

         CALL LJUST ( NUMSTR, NUMSTR )

         LINE         = ' '
         LINE(START:) = NUMSTR

         CALL SUFFIX ( ')', 1, LINE )

         CALL WRITLN ( LINE, UNIT )

      END IF

      CALL WRITLN ( ' ', UNIT )



!
!     Prepare to write the coefficients.
!
!
!     Perform some simple checks on the coefficients.
!
      IF (  ( NCOEFF .LT. 1 ) .OR. ( NCOEFF .GT. MXCOEF ) ) THEN

         CALL SETMSG ( 'SCLK coefficient record count for file # ' &
         //            'must be in range 1 : # but was #.'         )
         CALL ERRCH  ( '#', FNAME                                  )
         CALL ERRINT ( '#', MXCOEF                                 )
         CALL ERRINT ( '#', NCOEFF                                 )
         CALL SIGERR ( 'SPICE(BADRECORDCOUNT)'                     )
         CALL CHKOUT ( 'T_WCLK01'                                  )
         RETURN

      END IF

!
!     The first tick value must be non-negative; the tick values
!     must be strictly increasing.
!
      IF ( COEFFS(1,1) .LT. 0.D0 ) THEN

         CALL SETMSG ( 'First tick coefficient  for file # ' &
         //            'must be non-negative but was #.'         )
         CALL ERRCH  ( '#', FNAME                                )
         CALL ERRDP  ( '#', COEFFS(1,1)                          )
         CALL SIGERR ( 'SPICE(NEGATIVETICK)'                     )
         CALL CHKOUT ( 'T_WCLK01'                                )
         RETURN

      END IF

      DO I = 2, NCOEFF

         IF ( COEFFS(1,I) .LE. COEFFS(1,I-1) ) THEN

            CALL SETMSG ( 'In file #, tick coefficient # ' &
            //            'at index # must be strictly less than ' &
            //            'coefficient # at index # but was not. '  )
            CALL ERRCH  ( '#', FNAME                                )
            CALL ERRDP  ( '#', COEFFS(1,I-1)                        )
            CALL ERRINT ( '#', I-1                                  )
            CALL ERRDP  ( '#', COEFFS(1,I)                          )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(TICKSOUTOFORDER)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                )
            RETURN

         END IF

      END DO

!
!     The parallel time system values must be strictly increasing.
!
      DO I = 2, NCOEFF

         IF ( COEFFS(2,I) .LE. COEFFS(2,I-1) ) THEN

            CALL SETMSG ( 'In file #, parallel time coefficient # ' &
            //            'at index # must be strictly less than ' &
            //            'coefficient # at index # but was not. '  )
            CALL ERRCH  ( '#', FNAME                                )
            CALL ERRDP  ( '#', COEFFS(2,I-1)                        )
            CALL ERRINT ( '#', I-1                                  )
            CALL ERRDP  ( '#', COEFFS(2,I)                          )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                )
            RETURN

         END IF

      END DO

!
!     The SCLK rate values must be strictly positive.
!
      DO I = 1, NCOEFF

         IF ( COEFFS(3,I) .LE. 0.D0 ) THEN

            CALL SETMSG ( 'In file #, SCLK rate coefficient # ' &
            //            'at index # must be strictly positive ' &
            //            'but was not. '                           )
            CALL ERRCH  ( '#', FNAME                                )
            CALL ERRDP  ( '#', COEFFS(3,I-1)                        )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(NONPOSITIVERATE)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                )
            RETURN

         END IF

      END DO

!
!     Write the coefficient data; three values per line.
!     The first line contains no data.
!
      LINE   = '   SCLK01_COEFFICIENTS_#         = ('
      CALL REPMI ( LINE, '#', STRID, LINE )

      CALL WRITLN ( LINE, UNIT )
      CALL WRITLN ( ' ',  UNIT )


      DO I = 1, NCOEFF
!
!        We must format coefficients using a Fortran internal write
!        because the SPICE formatting routines can't handle 16
!        significant digits.
!
         WRITE ( LINE, &
                 '(3X,F20.1,2E26.16)', &
                 IOSTAT = IOSTAT        )    ( COEFFS(J,I), J = 1, 3 )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Coefficient record # for file # could ' &
            //            'not be formatted; values = # # #; ' &
            //            'IOSTAT = #.'                            )
            CALL ERRINT ( '#', I                                   )
            CALL ERRCH  ( '#', FNAME                               )
            CALL ERRDP  ( '#', COEFFS(1,I)                         )
            CALL ERRDP  ( '#', COEFFS(2,I)                         )
            CALL ERRDP  ( '#', COEFFS(3,I)                         )
            CALL ERRINT ( '#', IOSTAT                              )
            CALL SIGERR ( 'SPICE(WRITEERROR)'                      )
            CALL CHKOUT ( 'T_WCLK01'                               )
            RETURN

         END IF

         CALL WRITLN ( LINE, UNIT )

      END DO


      LINE = '   )'

      CALL WRITLN ( LINE, UNIT )

!
!     Write a final "begintext" marker.
!
      CALL WRITLN ( ' ',       UNIT )
      CALL WRITLN ( BEGTXT(),  UNIT )
      CALL WRITLN ( ' ',       UNIT )

!
!     Finish the file.
!
      CLOSE ( UNIT )

      CALL CHKOUT ( 'T_WCLK01' )
      RETURN
      END




