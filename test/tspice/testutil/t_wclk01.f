C$Procedure T_WCLK01 ( Write a type 1 SCLK kernel )
 
      SUBROUTINE T_WCLK01 ( FNAME,  COMS,   CLKID,  KERID, TIMSYS, 
     .                      NFIELD, MODULI, OFFSET, DELIM, NPARTS, 
     .                      PSTART, PSTOP,  NCOEFF, COEFFS        )
 
C$ Abstract
C
C     Write a type 1 SCLK kernel whose kernel variable assignments are
C     based on data passed in by the caller.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SCLK
C
C$ Keywords
C
C     SCLK
C     TEST 
C     UTILITY
C
C$ Declarations
 
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

 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   SCLK kernel name.
C     COMS       I   Kernel comment block.
C     CLKID      I   SCLK ID code.
C     KERID      I   Kernel ID: date in text kernel format.
C     TIMSYS     I   Time system name.
C     NFIELD     I   Number of fields in SCLK string format.
C     MODULI     I   Moduli of SCLK string format.
C     OFFSET     I   Offsets of SCLK string format.
C     DELIM      I   SCLK string output delimiter.
C     NPARTS     I   Number of partitions.
C     PSTART     I   Partition start times.
C     PSTOP      I   Partition stop times.
C     NCOEFF     I   Number of SCLK coefficient records.
C     COEFFS     I   SCLK coefficient records.
C
C$ Detailed_Input
C
C     FNAME      is the name of the output SCLK kernel to create.
C                This kernel must not already exist.
C
C     COMS       is a SPICELIB character cell containing a block
C                of comments to be inserted in the SCLK kernel 
C                after the ID word and before the first "begindata"
C                marker.
C
C     CLKID      is the integer ID code of the clock with which
C                the output kernel is associated. This ID normally
C                is the same as the integer ID of the spacecraft
C                on which the clock is mounted.
C
C     KERID      is the SCLK Kernel ID string. This is a
C                date in text kernel format: The date starts
C                with the character '@' and contains no embedded
C                blanks. Example:
C
C                   @01-JAN-2000/12:00:00
C
C     TIMSYS     is the name of the parallel time system referenced
C                by the clock correlation coefficients. Supported
C                values are 
C
C                   'TDB'
C                   'TDT'
C
C                case and blanks are not significant in TIMSYS.
C
C     NFIELD     is the number of fields in SCLK string format.
C
C     MODULI     is an array containing the moduli of SCLK string
C                format. The order of the moduli in the array
C                corresponds to left-to-right order in the SCLK string
C                format.
C
C     OFFSET     is an array containing the offsets of SCLK string
C                format. The order of the offsets in the array
C                corresponds to left-to-right order in the SCLK string
C                format.
C
C     DELIM      is the SCLK string output delimiter. This delimiter
C                is used by the SCLK system to create SCLK strings.
C                The delimiter must belong to the set contained in
C                the parameter DELIMS declared in the INCLUDE file
C                sclk.inc.
C
C     NPARTS     is the number of partitions for the SCLK data set.
C                NPARTS is used to define the dimensions of the 
C                input arrays PSTART and PSTOP.
C
C     PSTART     is an array of partition start times. PSTART has 
C                dimension NPARTS.
C
C     PSTOP      is an array of partition stop times.  PSTOP has 
C                dimension NPARTS.
C
C     NCOEFF     is the number of SCLK coefficient records in the 
C                array COEFFS.
C
C     COEFFS     is a 3 x NCOEFF array of SCLK coefficient records. 
C                The elements having indices
C                 
C                   (1,I), (2,I), (3,I)
C    
C                are, respectively, the Ith tick coefficient, the Ith
C                parallel time system coefficient, and the Ith clock
C                rate coefficient.
C
C$ Detailed_Output
C
C     None. See the section Files below.
C
C$ Parameters
C
C     See the INCLUDE file sclk.inc.
C
C$ Exceptions
C
C     1)  If the file designated by FNAME exists, the error
C         SPICE(FILEEXISTS) is signaled.
C
C     2)  If the SCLK kernel ID string does not contain
C         a date in SPICE text kernel format, the error
C         SPICE(INVALIDIDSTRING) is signaled.
C
C     3)  If the input time system name is not equivalent
C         to (ignoring case and blanks) a supported value,
C         the error SPICE(INVALIDTIMESYSTEM) is signaled.
C         
C     4)  If the SCLK string field count is out of range,
C         the error SPICE(INVALIDFIELDCOUNT) is signaled.
C
C     5)  If any SCLK field modulus is out of range,
C         the error SPICE(INVALIDMODULUS) is signaled.
C
C     6)  If the product of the SCLK field moduli is out of range,
C         the error SPICE(INVALIDMODULI) is signaled.
C
C     7)  If any SCLK field offset is out of range,
C         the error SPICE(INVALIDOFFSET) is signaled.
C
C     8)  If the output SCLK string field delimiter is not
C         a supported value, the error SPICE(INVALIDDELIMITER) 
C         is signaled.
C
C     9)  If the partition count is out of range,
C         the error SPICE(BADPARTITIONCOUNT) is signaled.
C
C     10) If any partition start time is out of range,
C         the error SPICE(BADPARTITIONSTART) is signaled.
C
C     11) If any partition start time is non-integral,
C         the error SPICE(BADPARTITIONSTART) is signaled.
C
C     12) If any partition start time cannot be converted to a
C         string via a Fortran WRITE, the error 
C         SPICE(WRITEERROR) is signaled.
C
C     13) If any partition stop time is out of range,
C         the error SPICE(BADPARTITIONSTOP) is signaled.
C
C     14) If any partition stop time is non-integral,
C         the error SPICE(BADPARTITIONSTOP) is signaled.
C
C     15) If any partition stop time cannot be converted to a
C         string via a Fortran WRITE, the error 
C         SPICE(WRITEERROR) is signaled.
C
C     16) If the SCLK coefficient record count is out of range,
C         the error SPICE(BADRECORDCOUNT) is signaled.
C
C     17) If the first tick coefficient is negative, the 
C         error SPICE(NEGATIVETICK) is signaled.
C
C     18) If the tick coefficients are not in strictly increasing
C         order, the error SPICE(TICKSOUTOFORDER) is signaled.
C
C     19) If the parallel time system coefficients are not in strictly
C         increasing order, the error SPICE(TIMESOUTOFORDER) is
C         signaled.
C
C     20) If any SCLK rate value is non-positive, the error
C         SPICE(NONPOSITIVERATE) is signaled.
C
C     21) If 3-element coefficient record cannot be converted to a
C         string via a Fortran WRITE, the error 
C         SPICE(WRITEERROR) is signaled.
C
C$ Files
C
C     The output of this routine is the SCLK kernel named
C     FNAME having contents provided by the input arguments
C     to this routine.     
C
C     No leapseconds kernel need be loaded in order to use this
C     routine.
C
C$ Particulars
C
C     This routine is intended to support creation of SCLK kernels
C     for testing. The routine could be used as a component of
C     a general purpose SCLK kernel writing routine. 
C
C$ Examples
C
C     1) The following program creates the SCLK kernel shown
C        at the bottom of this header section:
C
C
C        PROGRAM TWRT
C        IMPLICIT NONE
C  C
C  C     SPICELIB Functions
C  C
C        LOGICAL               EXISTS
C  C
C  C     Local parameters
C  C
C        CHARACTER*(*)         FNAME
C        PARAMETER           ( FNAME  = 'wrt.tsc' )
C
C        INTEGER               LBCELL
C        PARAMETER           ( LBCELL = -5 )
C
C        INTEGER               NCOM
C        PARAMETER           ( NCOM   = 3 )
C
C        CHARACTER*(*)         DELIM
C        PARAMETER           ( DELIM  = ':' )
C
C        INTEGER               NFIELD
C        PARAMETER           ( NFIELD = 4 )
C
C        INTEGER               NPARTS
C        PARAMETER           ( NPARTS = 3 )
C
C        INTEGER               NCOEFF
C        PARAMETER           ( NCOEFF = 10 )
C
C        INTEGER               LNSIZE
C        PARAMETER           ( LNSIZE = 80 )
C
C  C
C  C     Local variables
C  C
C        CHARACTER*(LNSIZE)    COMS ( LBCELL : NCOM )
C        CHARACTER*(LNSIZE)    LINE
C
C        DOUBLE PRECISION      COEFFS ( 3, NCOEFF )
C        DOUBLE PRECISION      DELTA
C        DOUBLE PRECISION      MODULI ( NFIELD )
C        DOUBLE PRECISION      OFFSET ( NFIELD )
C        DOUBLE PRECISION      PSTART ( NPARTS )
C        DOUBLE PRECISION      PSTOP  ( NPARTS )
C
C        INTEGER               I
C
C
C        IF ( EXISTS(FNAME) ) THEN
C           CALL DELFIL ( FNAME )
C        END IF
C
C        CALL SSIZEC ( NCOM, COMS )
C
C        DO I = 1, NCOM
C
C           LINE = '>>>>>>>> Comment line # <<<<<<<<<<<<'
C           CALL REPMI ( LINE, '#', I, LINE )
C
C           CALL APPNDC ( LINE, COMS )
C
C        END DO
C
C        MODULI(1) = 1000000
C        MODULI(2) =      10
C        MODULI(3) =       9
C        MODULI(4) =       8
C
C        OFFSET(1) =       1
C        OFFSET(2) =       2
C        OFFSET(3) =       3
C        OFFSET(4) =       4
C
C        DO I = 1, NPARTS
C           PSTART(I) = I
C           PSTOP (I) = 720000 + I
C        END DO
C
C        DELTA = 1.D-9
C
C        DO I = 1, NCOEFF
C
C           COEFFS(1,I) = (I-1) * 180000.D0
C
C           COEFFS(2,I) = (I-1) *  90000.D0
C
C           COEFFS(3,I) = 2.D0  +  I*DELTA
C
C        END DO
C
C        CALL T_WCLK01 ( FNAME,
C       .                COMS,
C       .                -99,
C       .                '@01-JAN-2000/12:00:00',
C       .                'TDB',
C       .                NFIELD,
C       .                MODULI,
C       .                OFFSET,
C       .                DELIM,
C       .                NPARTS,
C       .                PSTART,
C       .                PSTOP,
C       .                NCOEFF,
C       .                COEFFS       )
C
C        END
C
C
C     The SCLK kernel shown below has been modified slightly
C     to fit within the 72-column limit imposed by standard
C     Fortran 77 format restrictions: the SCLK coefficients
C     assignments have been shifted left.
C
C
C     KPL/SCLK
C
C     >>>>>>>> Comment line 1 <<<<<<<<<<<<
C     >>>>>>>> Comment line 2 <<<<<<<<<<<<
C     >>>>>>>> Comment line 3 <<<<<<<<<<<<
C
C     \begindata
C
C        SCLK_KERNEL_ID                  = ( @01-JAN-2000/12:00:00 )
C
C        SCLK_DATA_TYPE_99               = ( 1 )
C
C        SCLK01_TIME_SYSTEM_99           = ( 1 )
C        SCLK01_N_FIELDS_99              = ( 4 )
C        SCLK01_MODULI_99                = ( 1000000 10 9 8 )
C        SCLK01_OFFSETS_99               = ( 4 3 2 1 )
C        SCLK01_OUTPUT_DELIM_99          = ( 2 )
C
C        SCLK_PARTITION_START_99         = ( 1.0,
C                                            2.0,
C                                            3.0 )
C
C        SCLK_PARTITION_END_99           = ( 720001.0,
C                                            720002.0,
C                                            720003.0 )
C
C        SCLK01_COEFFICIENTS_99          = (
C
C                0.0    0.0000000000000000E+00    0.2000000001000000E+01
C           180000.0    0.9000000000000000E+05    0.2000000002000000E+01
C           360000.0    0.1800000000000000E+06    0.2000000003000000E+01
C           540000.0    0.2700000000000000E+06    0.2000000004000000E+01
C           720000.0    0.3600000000000000E+06    0.2000000005000000E+01
C           900000.0    0.4500000000000000E+06    0.2000000006000000E+01
C          1080000.0    0.5400000000000000E+06    0.2000000007000000E+01
C          1260000.0    0.6300000000000000E+06    0.2000000008000000E+01
C          1440000.0    0.7200000000000000E+06    0.2000000009000000E+01
C          1620000.0    0.8100000000000000E+06    0.2000000010000000E+01
C        )
C
C     \begintext
C
C
C
C$ Restrictions
C
C     1) This routine supports only type 1 spacecraft clocks.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 31-JAN-2014 (BVS)
C
C        BUG FIX: changed (MODULI(I) .LT. 2) to (MODULI(I) .LT. 1.D0)
C        as comparison of a DP must be done to a DP value and the
C        smallest allowed moduli value is 1.
C
C-    TSPICE Version 1.0.0, 21-FEB-2008 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               LASTNB

      LOGICAL               EQSTR
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Test utility functions
C
      EXTERNAL              BEGDAT
      CHARACTER*(80)        BEGDAT

      EXTERNAL              BEGTXT
      CHARACTER*(80)        BEGTXT

C
C     Local Parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 200 )

C
C     Local Variables
C
C
C     We use a longer, non-standard length for ABCORR because we
C     want to include embedded blanks for testing.
C
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

C
C     Saved variables
C
      SAVE                  DLCHRS

C
C     Initial values
C
      DATA                  DLCHRS / DELIMS /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_WCLK01' )

C
C     The clock ID to use in strings is the negative of
C     the input ID.
C
      STRID = -CLKID

C
C     Don't overwrite an existing file.
C
      IF ( EXISTS(FNAME) ) THEN

         CALL SETMSG ( 'File # already exists.' )
         CALL ERRCH  ( '#', FNAME               )
         CALL SIGERR ( 'SPICE(FILEEXISTS)'      )
         CALL CHKOUT ( 'T_WCLK01'               )
         RETURN

      END IF

C
C     Open the SCLK kernel as a new text file.
C
      CALL TXTOPN ( FNAME, UNIT )
      
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_WCLK01' )
         RETURN
      END IF

C
C     Write the SCLK ID word.
C
      CALL WRITLN ( ' ',         UNIT )
      CALL WRITLN ( 'KPL/SCLK',  UNIT )
      CALL WRITLN ( ' ',         UNIT )

C
C     Write any initial comments that may have been
C     supplied.
C
      DO I = 1, CARDC(COMS) 
         CALL WRITLN ( COMS(I), UNIT )
      END DO

C
C     Write the "begindata" marker.
C
      CALL WRITLN ( ' ',       UNIT )
      CALL WRITLN ( BEGDAT(),  UNIT )
      CALL WRITLN ( ' ',       UNIT )

C
C     Write the kernel ID. The kernel ID must be a date
C     of the form
C
C        @<date>
C
C     where <date contains no embedded blanks.
C     
      CALL LJUST ( KERID, LINE )
      L = LASTNB(LINE)

      IF ( L .EQ. 0 ) THEN

         CALL SETMSG ( 'Kernel ID written to file # must be '
     .   //            'non-blank.' )
         CALL ERRCH  ( '#', FNAME                )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'  )
         CALL CHKOUT ( 'T_WCLK01'                )
         RETURN
         
      END IF

      IF (  INDEX( LINE(:L), ' ' )  .NE.  0  )  THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must '
     .   //            'contain no embedded blanks'           )
         CALL ERRCH  ( '#', KERID                             )
         CALL ERRCH  ( '#', FNAME                             )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'               )
         CALL CHKOUT ( 'T_WCLK01'                             )
         RETURN
         
      END IF


      IF ( LINE(1:1) .NE. '@' ) THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must '
     .   //            'be of the form @<date> where date ' 
     .   //            'contains no embedded blanks.'       )
         CALL ERRCH  ( '#', KERID                           )
         CALL ERRCH  ( '#', FNAME                           )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'             )
         CALL CHKOUT ( 'T_WCLK01'                           )
         RETURN

      END IF

      IF ( L .LT. 9 ) THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must '
     .   //            'be of the form @<date> where date ' 
     .   //            'contains no embedded blanks.'       )
         CALL ERRCH  ( '#', KERID                           )
         CALL ERRCH  ( '#', FNAME                           )
         CALL SIGERR ( 'SPICE(INVALIDIDSTRING)'             )
         CALL CHKOUT ( 'T_WCLK01'                           )
         RETURN

      END IF

      CALL TPARSE ( LINE(2:L), SP2000, ERRMSG )

      IF ( ERRMSG .NE. ' ' ) THEN

         CALL SETMSG ( 'Kernel ID <#> written to file # must '
     .   //            'be of the form @<date> where date ' 
     .   //            'contains no embedded blanks. TPARSE '
     .   //            'returned message < # > '            )
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

C
C     Write the SCLK data type: for this routine, the value
C     is always 1.
C
      LINE = '   SCLK_DATA_TYPE_#              = ( 1 )'
      CALL REPMI ( LINE, '#', STRID, LINE )

      CALL WRITLN ( LINE, UNIT )
      CALL WRITLN ( ' ',  UNIT )

C
C     Write the time system code.
C      
      IF (  EQSTR( TIMSYS, 'TDB')  ) THEN

         TIMCDE = 1

      ELSE IF (  EQSTR( TIMSYS, 'TDT')  ) THEN
         
         TIMCDE = 2

      ELSE

         CALL SETMSG ( 'SCLK time system for file # must be '
     .   //            'TDB or TDT but was #.'               )
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

C
C     No blank line follows this output because the clock
C     attribute parameters are written as a contiguous
C     block. Why?  ...tradition.
C
      CALL WRITLN ( LINE, UNIT )

C
C     Write the number of fields.
C
      IF ( ( NFIELD .LT. 1 ) .OR. ( NFIELD .GT. MXNFLD )  ) THEN

         CALL SETMSG ( 'SCLK field count for file # must be '
     .   //            'in range 1:# but was #.'             )
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

C
C     Write the moduli. Recall these are d.p. numbers.
C
      DO I = 1, NFIELD
C
C        All moduli must be in the range 1 : 1e14.
C
         IF (      ( MODULI(I) .LT. 1.D0  ) 
     .        .OR. ( MODULI(I) .GT. 1.D14 )  ) THEN

            CALL SETMSG ( 'SCLK modulus # for file # must be '
     .      //            'in range 1 : # but was #.'           )
            CALL ERRINT ( '#', I                                )
            CALL ERRCH  ( '#', FNAME                            )
            CALL ERRDP  ( '#', 1.D14                            )
            CALL ERRDP  ( '#', MODULI(I)                        )
            CALL SIGERR ( 'SPICE(INVALIDMODULUS)'               )
            CALL CHKOUT ( 'T_WCLK01'                            )
            RETURN

         END IF

      END DO

C
C     Make sure the product of the moduli can fit in the d.p. mantissa
C     without truncation.
C
      PRODM = MODULI(1)

      DO I = 2, NFIELD
         PRODM = PRODM * MODULI(I)
      END DO
      
      IF ( PRODM .GT. 1.D15 ) THEN

         CALL SETMSG ( 'SCLK modulus product for file # must be '
     .   //            'in range # : # but was #. (Lower bound '
     .   //            'is 2**NFIELD.)'                         )
         CALL ERRCH  ( '#', FNAME                               )
         CALL ERRINT ( '#', 2**NFIELD                           )
         CALL ERRDP  ( '#', 1.D15                               )
         CALL ERRDP  ( '#', PRODM                               )
         CALL SIGERR ( 'SPICE(INVALIDMODULI)'                   )
         CALL CHKOUT ( 'T_WCLK01'                               )
         RETURN

      END IF

C
C     If we have 4 or fewer fields, write the moduli out
C     on one line on the right hand side. Otherwise
C     stack up the values on multiple lines.
C
C     Build the line in a long string; check the length
C     before writing the line.
C

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

C
C     The following case is for moduli assignments that
C     span multiple lines.
C
      IF (  ( NFIELD .GT. 4 ) .OR. ( LNB .GT. 80 )  ) THEN
C
C        Save the start position for the moduli.
C
         START = LASTNB(LONGLN)
C
C
C        Fill in the first line and write it.
C
         CALL DPFMT  ( MODULI(1), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                      NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

C
C        Append a comma to the line.
C
         CALL SUFFIX ( ',', 0, LONGLN )

         CALL WRITLN ( LONGLN, UNIT )

C
C        Fill in the lines preceding the last line and
C        write these as well.
C
         DO I = 2, NFIELD-1

            LONGLN         = ' '
            LONGLN(START:) = '#,'

            CALL DPFMT  ( MODULI(I), 'XXXXXXXXXXXXXX', NUMSTR )
            CALL LJUST  ( NUMSTR,                      NUMSTR )
            CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

            CALL WRITLN ( LONGLN, UNIT )

         END DO

C
C        Fill in and write the last line.
C
         LONGLN         = ' '
         LONGLN(START:) = '# )'

         CALL DPFMT  ( MODULI(NFIELD), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                           NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         CALL WRITLN ( LONGLN, UNIT )

      END IF
 

C
C     Write the offsets. Recall these are d.p. numbers.
C     The method is the same as for the moduli, except for
C     the range check.
C
      DO I = 1, NFIELD
C
C        All offsets must be in the range 0 : moduli(i)-1.
C
         IF (      ( OFFSET(I) .LT. 0.D0        ) 
     .        .OR. ( OFFSET(I) .GT. MODULI(1)-1 )  ) THEN

            CALL SETMSG ( 'SCLK offset # for file # must be '
     .      //            'in range 0 : # but was #.'           )
            CALL ERRINT ( '#', I                                )
            CALL ERRCH  ( '#', FNAME                            )
            CALL ERRDP  ( '#', MODULI(I) - 1                    )
            CALL ERRDP  ( '#', OFFSET(I)                        )
            CALL SIGERR ( 'SPICE(INVALIDOFFSET)'                )
            CALL CHKOUT ( 'T_WCLK01'                            )
            RETURN

         END IF

      END DO

C
C     If we have 4 or fewer fields, write the offsets out
C     on one line on the right hand side. Otherwise
C     stack up the values on multiple lines.
C
C     Build the line in a long string; check the length
C     before writing the line.
C
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

C
C     The following case is for offset assignments that
C     span multiple lines.
C
      IF (  ( NFIELD .GT. 4 ) .OR. ( LNB .GT. 80 )  ) THEN
C
C        Save the start position for the offsets.
C
         START = LASTNB(LONGLN)
C
C
C        Fill in the first line and write it.
C
         CALL DPFMT  ( OFFSET(1), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                      NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

C
C        Append a comma to the line.
C
         CALL SUFFIX ( ',', 0, LONGLN )

         CALL WRITLN ( LONGLN, UNIT )

C
C        Fill in the lines preceding the last line and
C        write these as well.
C
         DO I = 2, NFIELD-1

            LONGLN         = ' '
            LONGLN(START:) = '#,'

            CALL DPFMT  ( OFFSET(I), 'XXXXXXXXXXXXXX', NUMSTR )
            CALL LJUST  ( NUMSTR,                      NUMSTR )
            CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

            CALL WRITLN ( LONGLN, UNIT )

         END DO

C
C        Fill in and write the last line.
C
         LONGLN         = ' '
         LONGLN(START:) = '# )'

         CALL DPFMT  ( OFFSET(NFIELD), 'XXXXXXXXXXXXXX', NUMSTR )
         CALL LJUST  ( NUMSTR,                           NUMSTR )
         CALL REPMC  ( LONGLN, '#', NUMSTR,  LONGLN   )

         CALL WRITLN ( LONGLN, UNIT )

      END IF
 
C
C     Write the output delimiter line.
C     
      J = INDEX ( DLCHRS, DELIM )

      IF ( J .EQ. 0 ) THEN

         CALL SETMSG ( 'SCLK output delimiter for file # must be '
     .   //            'in set {#} but was #.'                  )
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

C
C     A blank line separates the clock attributes from the
C     partition data.
C
      CALL WRITLN ( ' ', UNIT )

C
C     Write the partition start data; one value only per line.
C
      IF (  ( NPARTS .LT. 1 ) .OR. ( NPARTS .GT. MXPART ) ) THEN

         CALL SETMSG ( 'SCLK partition count for file # must be '
     .   //            'in range 1 : # but was #.'                 )
         CALL ERRCH  ( '#', FNAME                                  )
         CALL ERRINT ( '#', MXPART                                 )
         CALL ERRINT ( '#', NPARTS                                 )
         CALL SIGERR ( 'SPICE(BADPARTITIONCOUNT)'                  )
         CALL CHKOUT ( 'T_WCLK01'                                  )
         RETURN

      END IF
      
      DO I = 1, NPARTS
C
C        Partition start readouts must be at least zero
C        and must be strictly less than the corresponding
C        stop readouts.
C         
         IF (      ( PSTART(I) .LT. 0.D0     ) 
     .        .OR. ( PSTART(I) .GE. PSTOP(I) )  ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # must be '
     .      //            'in range 0 : # but was #.'                 )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTOP(I)-1                             )
            CALL ERRDP  ( '#', PSTART(I)                              )
            CALL SIGERR ( 'SPICE(BADPARTITIONSTART)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

         END IF

C
C        Partition start times must be integral.
C
         IF (  PSTART(I)  .NE.  DNINT( PSTART(I) )  ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # must be '
     .      //            'integral but was #.'                       )
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

C
C     Record the value start index in the line.
C
      START = LASTNB(LINE)

C
C     We must format partition bounds using a Fortran internal write
C     because the SPICE formatting routines can't handle 16
C     significant digits.
C
      WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTART(1)

      IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'SCLK partition start # for file # could '
     .      //            'not be formatted; value = #; IOSTAT = #.'  )
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
C
C        Write the rest of the partitions.
C
         DO I = 2, NPARTS-1

            WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTART(I)

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'SCLK partition start # for file # could '
     .         //            'not be formatted; value = #; IOSTAT = #.')
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

            CALL SETMSG ( 'SCLK partition start # for file # could '
     .      //            'not be formatted; value = #; IOSTAT = #.')
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


C
C     Write the partition stop data; one value only per line.
C      
      DO I = 1, NPARTS
C
C        Partition stop readouts must be strictly greater than the
C        partition start values (this has been checked) and must not
C        exceed the product of the moduli.
C         
         IF (  PSTOP(I) .GT. PRODM  ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # must be '
     .      //            'in range # : # but was #.'                 )
            CALL ERRINT ( '#', I                                      )
            CALL ERRCH  ( '#', FNAME                                  )
            CALL ERRDP  ( '#', PSTART(I)                              )
            CALL ERRDP  ( '#', PRODM                                  )
            CALL ERRDP  ( '#', PSTOP(I)                               )
            CALL SIGERR ( 'SPICE(BADPARTITIONSTOP)'                   )
            CALL CHKOUT ( 'T_WCLK01'                                  )
            RETURN

         END IF

C
C        Partition stop times must be integral.
C
         IF (  PSTOP(I)  .NE.  DNINT( PSTOP(I) )  ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # must be '
     .      //            'integral but was #.'                       )
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

C
C     Record the value start index in the line.
C
      START = LASTNB(LINE)

C
C     We must format partition bounds using a Fortran internal write
C     because the SPICE formatting routines can't handle 16
C     significant digits.
C
      WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTOP(1)

      IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'SCLK partition stop # for file # could '
     .      //            'not be formatted; value = #; IOSTAT = #.'  )
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
C
C        Write the rest of the partitions.
C
         DO I = 2, NPARTS-1

            WRITE ( NUMSTR, '(F20.1)', IOSTAT = IOSTAT ) PSTOP(I)

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'SCLK partition stop # for file # could '
     .         //            'not be formatted; value = #; IOSTAT = #.')
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

            CALL SETMSG ( 'SCLK partition stop # for file # could '
     .      //            'not be formatted; value = #; IOSTAT = #.')
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



C
C     Prepare to write the coefficients.
C
C
C     Perform some simple checks on the coefficients.
C
      IF (  ( NCOEFF .LT. 1 ) .OR. ( NCOEFF .GT. MXCOEF ) ) THEN

         CALL SETMSG ( 'SCLK coefficient record count for file # '
     .   //            'must be in range 1 : # but was #.'         )
         CALL ERRCH  ( '#', FNAME                                  )
         CALL ERRINT ( '#', MXCOEF                                 )
         CALL ERRINT ( '#', NCOEFF                                 )
         CALL SIGERR ( 'SPICE(BADRECORDCOUNT)'                     )
         CALL CHKOUT ( 'T_WCLK01'                                  )
         RETURN

      END IF

C
C     The first tick value must be non-negative; the tick values 
C     must be strictly increasing.
C
      IF ( COEFFS(1,1) .LT. 0.D0 ) THEN

         CALL SETMSG ( 'First tick coefficient  for file # '
     .   //            'must be non-negative but was #.'         )
         CALL ERRCH  ( '#', FNAME                                )
         CALL ERRDP  ( '#', COEFFS(1,1)                          )
         CALL SIGERR ( 'SPICE(NEGATIVETICK)'                     )
         CALL CHKOUT ( 'T_WCLK01'                                )
         RETURN

      END IF

      DO I = 2, NCOEFF

         IF ( COEFFS(1,I) .LE. COEFFS(1,I-1) ) THEN

            CALL SETMSG ( 'In file #, tick coefficient # '
     .      //            'at index # must be strictly less than '
     .      //            'coefficient # at index # but was not. '  )
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

C
C     The parallel time system values must be strictly increasing.
C
      DO I = 2, NCOEFF

         IF ( COEFFS(2,I) .LE. COEFFS(2,I-1) ) THEN

            CALL SETMSG ( 'In file #, parallel time coefficient # '
     .      //            'at index # must be strictly less than '
     .      //            'coefficient # at index # but was not. '  )
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

C
C     The SCLK rate values must be strictly positive.
C
      DO I = 1, NCOEFF

         IF ( COEFFS(3,I) .LE. 0.D0 ) THEN

            CALL SETMSG ( 'In file #, SCLK rate coefficient # '
     .      //            'at index # must be strictly positive '
     .      //            'but was not. '                           )
            CALL ERRCH  ( '#', FNAME                                )
            CALL ERRDP  ( '#', COEFFS(3,I-1)                        )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(NONPOSITIVERATE)'                  )
            CALL CHKOUT ( 'T_WCLK01'                                )
            RETURN

         END IF
         
      END DO

C
C     Write the coefficient data; three values per line.
C     The first line contains no data.
C      
      LINE   = '   SCLK01_COEFFICIENTS_#         = ('
      CALL REPMI ( LINE, '#', STRID, LINE )

      CALL WRITLN ( LINE, UNIT )
      CALL WRITLN ( ' ',  UNIT )

      
      DO I = 1, NCOEFF
C
C        We must format coefficients using a Fortran internal write
C        because the SPICE formatting routines can't handle 16
C        significant digits.
C
         WRITE ( LINE, 
     .           '(3X,F20.1,2E26.16)', 
     .           IOSTAT = IOSTAT        )    ( COEFFS(J,I), J = 1, 3 )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Coefficient record # for file # could '
     .      //            'not be formatted; values = # # #; '
     .      //            'IOSTAT = #.'                            )
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

C
C     Write a final "begintext" marker.
C
      CALL WRITLN ( ' ',       UNIT )
      CALL WRITLN ( BEGTXT(),  UNIT )
      CALL WRITLN ( ' ',       UNIT )

C
C     Finish the file.
C
      CLOSE ( UNIT )

      CALL CHKOUT ( 'T_WCLK01' )
      RETURN
      END




