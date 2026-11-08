!$Procedure      ZZDASRFR ( DAS, read file record )

      SUBROUTINE ZZDASRFR ( HANDLE, &
                            IDWORD, &
                            IFNAME, &
                            NRESVR, &
                            NRESVC, &
                            NCOMR, &
                            NCOMC  )

! $ Abstract
!
!     Return the contents of the file record of a specified DAS file.
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
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzddhman.inc'

      INTEGER               HANDLE
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         IFNAME
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     IDWORD     O   ID word.
!     IFNAME     O   DAS internal file name.
!     NRESVR     O   Number of reserved records in file.
!     NRESVC     O   Number of characters in use in reserved rec. area.
!     NCOMR      O   Number of comment records in file.
!     NCOMC      O   Number of characters in use in comment area.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle for a previously opened DAS file.
!
! $ Detailed_Output
!
!     IDWORD      is the `ID word' contained in the first eight
!                 characters of the file record.
!
!     IFNAME      is the internal file name of the DAS file.  The
!                 maximum length of the internal file name is 60
!                 characters.
!
!     NRESVR      is the number of reserved records in the DAS file
!                 specified by HANDLE.
!
!     NRESVC      is the number of characters in use in the reserved
!                 record area of the DAS file specified by HANDLE.
!
!     NCOMR       is the number of comment records in the DAS file
!                 specified by HANDLE.
!
!     NCOMC       is the number of characters in use in the comment area
!                 of the DAS file specified by HANDLE.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the file read attempted by this routine fails, the error
!        SPICE(DASFILEREADFAILED) will be signaled.
!
!     2) If the input file handle is invalid, the error will diagnosed
!        by a routine in the call tree of this routine.
!
!     3) If a logical unit cannot be obtained for the file designated
!        by HANDLE, the error will diagnosed by a routine in the call
!        tree of this routine.
!
!     4) If the file's binary format is unrecognized, the error will
!        diagnosed by a routine in the call tree of this routine.
!
!     5) If the file designated by HANDLE has non-native binary format,
!        and if any numeric components of the file record cannot be
!        translated to native format, the error will diagnosed
!        by a routine in the call tree of this routine.
!
! $ Files
!
!     See the description of HANDLE under $Detailed_Input.
!
! $ Particulars
!
!     This routine provides a convenient way of retrieving the
!     information contained in the file record of a DAS file.
!
! $ Examples
!
!     1)  Obtain the internal file name of an existing DAS file.
!
!
!            C
!            C     Open the file for reading.
!            C
!                  CALL DASOPR ( FNAME, HANDLE  )
!
!            C
!            C     Retrieve the internal file name and print it.
!            C
!
!                  CALL ZZDASRFR ( HANDLE,
!                 .                IDWORD,
!                 .                IFNAME,
!                 .                NRESVR,
!                 .                NRESVC,
!                 .                NCOMR,
!                 .                NCOMC  )
!
!
!                  WRITE (*,*) 'Internal file name is: '
!                  WRITE (*,*)  IFNAME
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
!     K.R. Gehringer (JPL)
!     N.J. Bachman   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB)
!
!        Based on DASRFR Version 1.0.0, 15-JUL-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read DAS file record
!     read DAS internal file name
!
!-&

! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

!
!     Parameters for positions of file record elements:
!

!
!     ID word begin and end:
!
      INTEGER               BEGIDW
      PARAMETER           ( BEGIDW = 1 )

      INTEGER               ENDIDW
      PARAMETER           ( ENDIDW = 8 )


!
!     Internal file name begin and end:
!
      INTEGER               BEGIFN
      PARAMETER           ( BEGIFN = 9 )

      INTEGER               ENDIFN
      PARAMETER           ( ENDIFN = 68 )

!
!     Reserved record count begin and end:
!
      INTEGER               BEGNRR
      PARAMETER           ( BEGNRR = 69 )

      INTEGER               ENDNRR
      PARAMETER           ( ENDNRR = 72 )

!
!     Reserved record character count begin and end:
!
      INTEGER               BEGNRC
      PARAMETER           ( BEGNRC = 73 )

      INTEGER               ENDNRC
      PARAMETER           ( ENDNRC = 76 )


!
!     Comment area record count begin and end:
!
      INTEGER               BEGNCR
      PARAMETER           ( BEGNCR = 77 )

      INTEGER               ENDNCR
      PARAMETER           ( ENDNCR = 80 )

!
!     Comment area character count begin and end:
!
      INTEGER               BEGNCC
      PARAMETER           ( BEGNCC = 81 )

      INTEGER               ENDNCC
      PARAMETER           ( ENDNCC = 84 )

!
!     Local variables
!
      CHARACTER*(NWC)       CHRBUF
      CHARACTER*(IDWLEN)    TMPIDW
      CHARACTER*(IFNLEN)    TMPIFN

      INTEGER               IBFF
      INTEGER               IOSTAT
      INTEGER               NATBFF
      INTEGER               UNIT

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  NATBFF

!
!     Initial values
!
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / -1     /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDASRFR' )

!
!     On the first pass through this routine, get the integer code of
!     the host system's native binary file format.
!
      IF ( FIRST ) THEN

         CALL ZZDDHNFC ( NATBFF )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'ZZDASRFR' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     Get a logical unit for this DAS file.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

!
!     Get the integer code for the file's binary format.
!
      CALL ZZDDHPPF ( UNIT, DAS, IBFF )

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'ZZDASRFR' )
         RETURN
      END IF

      IF ( IBFF .EQ. NATBFF ) THEN
!
!        We're looking at a native file. Just read the file record.
!
         READ ( UNIT, &
                REC     =  1, &
                IOSTAT  =  IOSTAT ) TMPIDW, &
                                    TMPIFN, &
                                    NRESVR, &
                                    NRESVC, &
                                    NCOMR, &
                                    NCOMC

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Could not DAS read file record. File was ' &
            //            '#.  IOSTAT was #.'                        )
            CALL ERRFNM ( '#', UNIT                                  )
            CALL ERRINT ( '#', IOSTAT                                )
            CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                 )
            CALL CHKOUT ( 'ZZDASRFR'                                 )
            RETURN

         END IF

         IDWORD  =  TMPIDW
         IFNAME  =  TMPIFN


      ELSE
!
!        The file is non-native.
!
!        We don't check the access mode of the file, because we're
!        not going to reject files that are open for writing.
!
!        We'll read the file record as a character string and then
!        pick it apart.
!
         READ ( UNIT, &
                REC     =  1, &
                IOSTAT  =  IOSTAT ) CHRBUF

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Could not read DAS file record. File is ' &
            //            '#. IOSTAT was #. File''s BFF integer ' &
            //            'code is #.'                              )
            CALL ERRFNM ( '#', UNIT                                 )
            CALL ERRINT ( '#', IOSTAT                               )
            CALL ERRINT ( '#', IBFF                                 )
            CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                )
            CALL CHKOUT ( 'ZZDASRFR'                                )
            RETURN

         END IF

!
!        Set the string output arguments.
!
         IDWORD = CHRBUF( BEGIDW : ENDIDW )
         IFNAME = CHRBUF( BEGIFN : ENDIFN )

!
!        The integer output arguments require translation.
!
         CALL ZZXLATEI ( IBFF,  CHRBUF( BEGNRR : ENDNRR ),  1,  NRESVR )
         CALL ZZXLATEI ( IBFF,  CHRBUF( BEGNRC : ENDNRC ),  1,  NRESVC )
         CALL ZZXLATEI ( IBFF,  CHRBUF( BEGNCR : ENDNCR ),  1,  NCOMR  )
         CALL ZZXLATEI ( IBFF,  CHRBUF( BEGNCC : ENDNCC ),  1,  NCOMC  )

      END IF

      CALL CHKOUT ( 'ZZDASRFR' )
      RETURN
      END
