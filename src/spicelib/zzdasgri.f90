!$Procedure ZZDASGRI ( DAS, get record, integer )

      SUBROUTINE ZZDASGRI ( HANDLE, RECNO, RECORD )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read DAS integer physical records from native or non-native DAS
!     files.
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
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'das.inc'

      INTEGER               HANDLE
      INTEGER               RECNO
      INTEGER               RECORD ( NWI )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     RECORD     O   Integer data read from record.
!     NWI        P   Number of integers in a single DAS integer record.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an open DAS file.
!
!     RECNO          is the number of a record in a DAS file.
!
! $ Detailed_Output
!
!     RECORD         is an integer array containing the elements of the
!                    specified record.
!
! $ Parameters
!
!     NWI            is the number of elements in a single DAS integer
!                    data record.
!
! $ Exceptions
!
!     1)  If the input file handle cannot be mapped to a file
!         name, the error SPICE(HANDLENOTFOUND) will be signaled. The
!         output argument RECORD will not be modified.
!
!     2)  If a read operation attempted by this routine fails, the
!         error SPICE(DASFILEREADFAILED) will be signaled.
!
!     3)  If an error occurs while attempting to translate non-native
!         integer data to native format, the error will be diagnosed by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine enables DAS routines to read double precision data
!     records from native or non-native DAS files.
!
!     This routine should be used to read only records that contain
!     integer data.
!
! $ Examples
!
!     See usage in DASRRI.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-FEB-2017 (NJB)
!
!-&

! $ Index_Entries
!
!     read DAS integer physical records for arbitrary BFF
!     read DAS integer records for native or non-native DAS
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
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

!
!     Local variables
!
      CHARACTER*(NWC)       CHRREC
      CHARACTER*(FILSIZ)    FNAME

      INTEGER               INTAMH
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               IOSTAT
      INTEGER               NATBFF
      INTEGER               UNIT

      LOGICAL               FIRST
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE                 FIRST
      SAVE                 INTBFF
      SAVE                 NATBFF


!
!     Initial values
!
      DATA                 FIRST  / .TRUE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDASGRI' )

      IF ( FIRST ) THEN
!
!        Get the integer code for the host binary file format.
!
         CALL ZZDDHNFC ( NATBFF )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'ZZDASGRI' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     Get a logical unit for this file.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
!
!     Get the binary file format of the file designated by HANDLE.
!
      CALL ZZDDHNFO ( HANDLE, FNAME, INTARC, INTBFF, INTAMH, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZDASGRI' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Unable to locate file associated with ' &
         //            'HANDLE, #. The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         CALL ERRINT ( '#', HANDLE                                  )
         CALL SIGERR ( 'SPICE(HANDLENOTFOUND)'                      )
         CALL CHKOUT ( 'ZZDASGRI'                                   )
         RETURN

      END IF


      IF ( INTBFF .EQ. NATBFF ) THEN
!
!        The file has native format.
!
         READ (  UNIT    =   UNIT, &
                 REC     =   RECNO, &
                 IOSTAT  =   IOSTAT  ) RECORD

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Could not read DAS integer record. ' &
            //            'File = # Record number = #. IOSTAT = #.'    )
            CALL ERRFNM ( '#', UNIT                                    )
            CALL ERRINT ( '#', RECNO                                   )
            CALL ERRINT ( '#', IOSTAT                                  )
            CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                   )
            CALL CHKOUT ( 'ZZDASGRI'                                   )
            RETURN

         END IF

      ELSE
!
!        Read the record as a character string, then translate it
!        to an array of integers.
!
         READ (  UNIT    =    UNIT, &
                 REC     =    RECNO, &
                 IOSTAT  =    IOSTAT  ) CHRREC

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Could not read non-native DAS integer ' &
            //            'record into character array. ' &
            //            'File = # Record number = #. IOSTAT = #.'    )
            CALL ERRFNM ( '#', UNIT                                    )
            CALL ERRINT ( '#', RECNO                                   )
            CALL ERRINT ( '#', IOSTAT                                  )
            CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                   )
            CALL CHKOUT ( 'ZZDASGRI'                                   )
            RETURN

         END IF

!
!        Translate the character record to integer type.
!
         CALL ZZXLATEI ( INTBFF, CHRREC, NWI, RECORD )

!
!        We don't test FAILED here because the routine
!        will return from this point.
!
      END IF

      CALL CHKOUT ( 'ZZDASGRI' )
      RETURN
      END
