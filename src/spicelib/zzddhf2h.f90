!$Procedure ZZDDHF2H ( Private --- DDH Filename to Handle )

      SUBROUTINE ZZDDHF2H ( FNAME,  FTABS,  FTAMH, FTARC, FTBFF, &
                            FTHAN,  FTNAM,  FTRTM, FTMNM, NFT,   UTCST, &
                            UTHAN,  UTLCK,  UTLUN, NUT,   EXISTS, &
                            OPENED, HANDLE, FOUND, MNM                 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Convert filename to a handle.
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

      CHARACTER*(*)         FNAME

      INTEGER               FTABS ( * )
      INTEGER               FTAMH ( * )
      INTEGER               FTARC ( * )
      INTEGER               FTBFF ( * )
      INTEGER               FTHAN ( * )
      CHARACTER*(*)         FTNAM ( * )
      INTEGER               FTRTM ( * )
      DOUBLE PRECISION      FTMNM ( * )
      INTEGER               NFT

      INTEGER               UTCST ( * )
      INTEGER               UTHAN ( * )
      LOGICAL               UTLCK ( * )
      INTEGER               UTLUN ( * )
      INTEGER               NUT

      LOGICAL               EXISTS
      LOGICAL               OPENED
      INTEGER               HANDLE
      LOGICAL               FOUND
      DOUBLE PRECISION      MNM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of the file to convert to a handle.
!     FTABS,
!     FTAMH,
!     FTARC,
!     FTBFF,
!     FTHAN,
!     FTNAM,
!     FTRTM,
!     FTMNM      I   File table.
!     NFT        I   Number of entries in the file table.
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN     I/O  Unit table.
!     NUT       I/O  Number of entries in the unit table.
!     EXISTS     O   Logical indicating if FNAME exists.
!     OPENED     O   Logical indicating if FNAME is opened.
!     HANDLE     O   Handle associated with FNAME.
!     FOUND      O   Logical indicating if FNAME's HANDLE was found.
!     MNM        O   Unique DP (Magic NuMber) associated with FNAME.
!
! $ Detailed_Input
!
!     FNAME      is the name of the file to locate in the file table.
!
!     FTABS,
!     FTAMH,
!     FTARC,
!     FTBFF,
!     FTHAN,
!     FTNAM,
!     FTRTM,
!     FTMNM      are the arrays respectively containing the absolute
!                value of the handle, access method, architecture,
!                binary file format, handle, name, RTRIM and
!                magic number columns of the file table.
!
!     NFT        is the number of entries in the file table.
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN      are the arrays respectively containing the cost,
!                handle, locked, and logical unit columns of the unit
!                table.
!
!     NUT        is the number of entries in the unit table.
!
! $ Detailed_Output
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN      are the arrays respectively containing the cost,
!                handle, locked, and logical unit columns of the unit
!                table.  If ZZDDHF2H requires a logical unit, then
!                it will borrow one from the unit table.  Depending
!                on the state of the table passed in from the caller
!                one of three possible scenarios may occur (Recall
!                that 'zero-cost' rows are ones whose units are
!                reserved with RESLUN and not currently connected
!                to any file.)
!
!                   A 'zero-cost' row exists in the table, in
!                   which case the row is used temporarily and
!                   may be removed depending on the number of entries
!                   in the file table (NFT).
!
!                   The unit table is full (NUT=UTSIZE), in which
!                   case the unit with the lowest cost that is not
!                   locked to its handle will be disconnected, used,
!                   and then returned to the table as a 'zero-cost'
!                   row before returning to the caller.
!
!                   The unit table is not full (NUT<UTSIZE) and there
!                   are no 'zero-cost' rows.  In this case NUT is
!                   temporarily increased by one, and the new row
!                   is used.  After this routine no longer requires
!                   the unit, depending on the number of entries in
!                   the file table (NFT) the row may be left in the
!                   table as a 'zero-handle' row or removed entirely.
!
!                In the event an error is signaled, the contents of the
!                unit table are placed into a usable state before
!                returning to the caller.
!
!     NUT        is the number of entries in the unit table. Since
!                this routine borrows a unit from the unit table, which
!                may involve allocation of a new unit, this value may
!                change.
!
!     EXISTS     is a logical if set to TRUE, indicates that FNAME
!                exists.  If FALSE, FNAME does not exist.  In the event
!                an exception is signaled the value is undefined.
!
!     OPENED     is a logical if set to TRUE, indicates that FNAME
!                is opened and attached to a logical unit.  If FALSE,
!                FNAME is not attached to a unit.  In the event an
!                exception is signaled the value is undefined.
!
!     HANDLE     is the handle in the file table associated with
!                FNAME.  If FOUND is FALSE, then HANDLE is returned as
!                0.
!
!     FOUND      is a logical if TRUE indicates that FNAME was found
!                in the file table.  If FALSE indicates that it was not
!                located.
!
!     MNM        is a unique (enough) DP number -- the Magic NuMber --
!                associated with FNAME computed by this examining the
!                file contents.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If any of the INQUIRE statements this routine performs fail,
!        the error SPICE(INQUIREFAILED) is signaled. FOUND is set to
!        FALSE and HANDLE to 0.
!
!     2) If the attempt to open FNAME fails, then SPICE(FILEOPENFAILED)
!        is signaled. FOUND is set to FALSE, and HANDLE to 0.
!
!     3) If FNAME is determined not to be loaded into the file table
!        then FOUND is set to FALSE and HANDLE is set to 0.
!
! $ Files
!
!     If the file named by FNAME is not connected to a logical unit,
!     this routine will open it for direct access to complete its
!     examination.
!
! $ Particulars
!
!     This routine encapsulates the logic necessary to determine if
!     a particular filename names a file already loaded into the
!     DAF/DAS handle manager.  If it discovers the file is loaded,
!     the routine returns the handle to the caller.
!
! $ Examples
!
!     See ZZDDHFNH for sample usage.
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
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 3.0.0, 26-APR-2012 (BVS)
!
!        Changed calling sequence to include FTMNM and MNM. Change
!        algorithm to compute MNM and use it to bypass n^2 INQUIREs
!        for files opened for READ access, if possible.
!
!-    SPICELIB Version 2.0.1, 24-APR-2003 (EDW)
!
!        Added MAC-OSX-F77 to the list of platforms
!        that require READONLY to read write protected
!        kernels.
!
!-    SPICELIB Version 2.0.0, 05-AUG-2002 (FST)
!
!        Bug fix: this module was updated to allow proper loading
!        of read-only files on VAX environments.
!
!-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
!
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.0.0, 05-AUG-2002 (FST)
!
!        An OPEN statement that is exercised by this module under
!        certain circumstances, failed to pass the non-standard
!        READONLY option for the VAX environments.  This had the
!        undesirable side-effect of not permitting files available
!        only for READ access to be opened.
!
!        This file was promoted from a standard portable module
!        to a master file.
!
!-&

!
!     SPICELIB Functions
!
      DOUBLE PRECISION      ZZDDHMNM

      INTEGER               ISRCHI
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               RCHAR
      INTEGER               UINDEX
      INTEGER               UNIT

      LOGICAL               LOCEXS
      LOGICAL               LOCOPN

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDDHF2H' )
      END IF

!
!     First check to see if FNAME is blank.  If so, set FOUND to .FALSE.
!     and return.  ZZDDHOPN prevents any blank filenames from being
!     loaded into the file table.
!
      IF ( FNAME .EQ. ' ' ) THEN

         FOUND  = .FALSE.
         HANDLE = 0
         OPENED = .FALSE.
         EXISTS = .FALSE.
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN

      END IF

!
!     Start by trimming the file name in preparation for the INQUIRE.
!
      RCHAR = RTRIM ( FNAME )

!
!     Now INQUIRE on the input file FNAME.
!
      INQUIRE ( FILE   = FNAME( 1:RCHAR ), &
                EXIST  = LOCEXS, &
                OPENED = LOCOPN, &
                NUMBER = UNIT, &
                IOSTAT = IOSTAT            )

!
!     Check IOSTAT for failure.
!
      IF ( IOSTAT .NE. 0 ) THEN
         FOUND  = .FALSE.
         HANDLE = 0
         CALL SETMSG ( 'INQUIRE failed. Value of IOSTAT was #.' )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
         CALL CHKOUT ( 'ZZDDHF2H'                               )
         RETURN
      END IF

!
!     First, set some of the output arguments.  Remember, some
!     systems consider non-existent files as open.  Compensate for
!     this unusual behavior.
!
      EXISTS = LOCEXS
      OPENED = LOCOPN .AND. EXISTS

!
!     Now check to see if the file exists.  If it does not, then
!     set FOUND to false and HANDLE to 0 as non-existent files
!     can not possibly be present in the file table.
!
      IF ( .NOT. EXISTS ) THEN

         FOUND  = .FALSE.
         HANDLE = 0
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN

      END IF

!
!     Now check to see if the file is opened.  If it is, we need to
!     determine whether or not the logical unit to which it is
!     attached is present in the unit table.
!
      IF ( OPENED ) THEN

!
!        Since the file is opened, see if we can find its unit
!        in the unit table.
!
         UINDEX = ISRCHI ( UNIT, NUT, UTLUN )

!
!        When UINDEX is 0, the file is opened, but not by
!        the DAF/DAS handle manager.  Set FOUND to FALSE, HANDLE
!        to 0, and return to the caller.
!
         IF ( UINDEX .EQ. 0 ) THEN
            HANDLE = 0
            FOUND  = .FALSE.
            CALL CHKOUT ( 'ZZDDHF2H' )
            RETURN
         END IF

!
!        If we end up here, then we found UNIT in the unit table.
!        Set FOUND to TRUE if the handle associated with UNIT is
!        non-zero.
!
         HANDLE = UTHAN(UINDEX)
         FOUND  = HANDLE .NE. 0
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN

      END IF

!
!     At this point, we took action for all simple cases.  Now
!     we need to find out if FNAME is one of the files in the
!     file table that isn't open.  To determine this, we open FNAME,
!     and then INQUIRE on every file in the table.  To do this, we
!     need a unit. Get one.
!
      CALL ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

      IF ( FAILED() ) THEN
         HANDLE = 0
         FOUND  = .FALSE.
         CALL CHKOUT ( 'ZZDDHF2H' )
         RETURN
      END IF

!
!     Now open the file (which we know exists and isn't open). Since
!     we effectively are just borrowing this unit, we are not going to
!     set UTHAN or UTCST from the defaults that ZZDDHGTU sets up.
!
      OPEN ( UNIT   = UTLUN(UINDEX), &
             FILE   = FNAME( 1:RCHAR ), &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'OLD', &
             IOSTAT = IOSTAT            )

!
!     Check IOSTAT.
!
      IF ( IOSTAT .NE. 0 ) THEN

!
!        Since an error has occurred, set FOUND to false and HANDLE
!        to 0.
!
         FOUND  = .FALSE.
         HANDLE = 0

!
!        Close the unit and remove it from the unit table.
!
         CLOSE ( UNIT = UTLUN(UINDEX) )

         CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK, UTLUN, NUT )

!
!        Signal the error and return.
!
         CALL SETMSG ( 'Attempt to open file ''#'' failed. Value of ' &
         //            'IOSTAT was #.'                                )
         CALL ERRCH  ( '#', FNAME                                     )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                        )
         CALL CHKOUT ( 'ZZDDHF2H'                                     )
         RETURN

      END IF

!
!     Get a unique enough DP number -- the Magic NuMber (MNM) ;) -- for
!     this file.
!
      MNM = ZZDDHMNM ( UTLUN(UINDEX) )

!
!     Now loop through all the files in the file table. Unfortunately
!     we have no other choice.
!
      I     = 1
      FOUND = .FALSE.

      DO WHILE ( ( I .LE. NFT ) .AND. ( .NOT. FOUND ) )

!
!        If this file's magic number is non-zero and is different from
!        the magic number of the currently checked, opened-for-READ
!        file, we will declare that these files are not the same file
!        and will skip INQUIRE. In all other cases we will do INQUIRE
!        and check UNITs.
!
         IF (   MNM .NE. 0.D0                               .AND. &
              ( MNM .NE. FTMNM(I) .AND. FTAMH(I) .EQ. READ )     ) THEN

!
!           These files are not the same file. Clear IOSTAT and set
!           UNIT to not match the UNIT of the input file.
!
            IOSTAT = 0
            UNIT   = UTLUN(UINDEX) + 1

         ELSE

!
!           Do the INQUIRE. ;(
!
            INQUIRE ( FILE   = FTNAM(I) ( 1 : FTRTM(I) ), &
                      EXIST  = LOCEXS, &
                      OPENED = LOCOPN, &
                      NUMBER = UNIT, &
                      IOSTAT = IOSTAT               )

         END IF

!
!        Check IOSTAT.
!
         IF ( IOSTAT .NE. 0 ) THEN

!
!           Since we have an error condition, set FOUND to FALSE
!           and HANDLE to 0.
!
            FOUND  = .FALSE.
            HANDLE = 0

!
!           Close the unit and clean up the unit table.
!
            CLOSE ( UNIT = UTLUN(UINDEX) )

            CALL ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                            UTLCK,  UTLUN, NUT           )


!
!           Signal the error and return.
!
            CALL SETMSG ( 'INQUIRE failed. Value of IOSTAT was #.' )
            CALL ERRINT ( '#', IOSTAT                              )
            CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
            CALL CHKOUT ( 'ZZDDHF2H'                               )
            RETURN

         END IF

!
!        Now check to see if FILE exists, is currently open. and
!        its UNIT matches UTLUN(UINDEX).
!
         IF (       ( LOCEXS .AND. LOCOPN ) &
              .AND. (   UNIT .EQ.  UTLUN(UINDEX) ) ) THEN

               HANDLE = FTHAN(I)
               FOUND  = .TRUE.

!
!        Otherwise, continue searching.
!
         ELSE
            I = I + 1
         END IF

      END DO

!
!     Check to see if we found the file in the file table.
!
      IF ( .NOT. FOUND ) THEN
         HANDLE = 0
      END IF

!
!     Close the unit and clean up the unit table.
!
      CLOSE ( UNIT=UTLUN(UINDEX) )

      CALL ZZDDHRMU ( UINDEX, NFT, UTCST, UTHAN, UTLCK,  UTLUN, NUT )

      CALL CHKOUT ( 'ZZDDHF2H' )
      RETURN

      END
