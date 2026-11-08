!$Procedure DASCUD ( DAS, create or update directories )

      SUBROUTINE DASCUD ( HANDLE, TYPE, NWORDS )

! $ Abstract
!
!     Create or update directories in a DAS file to reflect addition
!     of a specified number of words of a specified data type.
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

      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               NWORDS

      INTEGER               CHAR
      PARAMETER           ( CHAR   =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     TYPE       I   Data type specifier.
!     NWORDS     I   Number of words of data being added.
!     CHAR       P   Parameter indicating character data type.
!     DP         P   Parameter indicating double precision data type.
!     INT        P   Parameter indicating integer data type.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a DAS file open for writing.
!
!     TYPE     is a data type specifier. TYPE may be any of
!              the parameters
!
!                 CHAR
!                 DP
!                 INT
!
!              which indicate `character', `double precision',
!              and `integer' respectively.
!
!     NWORDS   is the number of words of data of the data type
!              indicated by TYPE whose addition to the indicated
!              DAS file is to be accounted for.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     CHAR,
!     DP,
!     INT      are data type specifiers which indicate
!              `character', `double precision', and `integer'
!              respectively. These parameters are used in
!              all DAS routines that require a data type
!              specifier as input.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE)
!         is signaled.
!
!     3)  If NWORDS is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine operates by side effects: the directories in the
!     indicated DAS file will be updated to reflect the addition of
!     the indicated number of words of the specified data type.
!     If necessary, a new directory record will be added to the file
!     to hold a new cluster descriptor.
!
!     In addition, the file summary for the indicated DAS file will be
!     updated with the new values of the descriptor location and last
!     logical address of the indicated type, as well as with the new
!     value of the free record pointer.
!
!     This routine is used by the DASADx routines: after each data
!     addition, they call this routine to update the directories of the
!     affected DAS file.
!
!     Normally, there will be no need for routines outside of SPICELIB
!     to call this routine directly. To add data to or update a DAS
!     file, the DASADx and DASUDx routines should be used; these
!     routines take care of directory creation and updates.
!
! $ Examples
!
!     1)  Update directories after writing N integer words to a
!         DAS file designated by HANDLE:
!
!             CALL DASCUD ( HANDLE, INT, N )
!
! $ Restrictions
!
!     1)  This routine is intended for use by the SPICELIB DAS routines.
!         Non-SPICELIB software normally will not need to call this
!         routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.5.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.4.0, 07-AUG-2006 (NJB)
!
!        Bug fix: added initialization of variable LTYPE to support
!                 operation under the Macintosh Intel Fortran
!                 compiler. Note that this bug did not affect
!                 operation of this routine on other platforms.
!
!-    SPICELIB Version 1.3.0, 16-JAN-2003 (NJB)
!
!        Bug fix: fixed previous bug fix.
!
!-    SPICELIB Version 1.2.0, 10-DEC-2002 (NJB)
!
!        Bug fix: now a new, empty directory record with valid
!        backward and forward pointers is written immediately
!        when it is created.
!
!-    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB)
!
!        Corrected title of permuted index entry section.
!
!-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!        Removed an unused variable.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     update DAS cluster directories
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.4.0 07-AUG-2006 (NJB)
!
!        Bug fix: added initialization of variable LTYPE to support
!                 operation under the Macintosh Intel Fortran
!                 compiler. Note that this bug did not affect
!                 operation of this routine on other platforms. The
!                 statement referencing the uninitialized variable
!                 was:
!
!           ELSE IF (       ( TYPE   .EQ. LTYPE )
!          .          .AND. ( DSCREC .GT. 0     )
!          .          .AND. ( LWORD  .LT. NWI   )  ) THEN
!
!
!        In the previous version of the code, LTYPE is uninitialized
!        when the DAS file is empty, which implies DSCREC is 0.
!        Otherwise LTYPE is initialized. So the value of the logical
!        expression is not affected by the uninitialized value of
!        LTYPE.
!
!        However, the Intel Fortran compiler for the Mac flags a runtime
!        error when the above code is exercised. So LTYPE is now
!        initialized to an invalid value prior to execution of this
!        code. If the invalid value is ever used, a runtime error
!        should result.
!
!
!-    SPICELIB Version 1.3.0 16-JAN-2003 (NJB)
!
!        Bug fix: fixed previous bug fix.
!
!
!        The offending line (#778) in previous version) of code is:
!
!           CALL DASWRI ( HANDLE, RECNO, DIRREC )
!
!        The correct line of code is:
!
!          CALL DASWRI ( HANDLE, FREE, DIRREC )
!
!
!-    SPICELIB Version 1.2.0 10-DEC-2002 (NJB)
!
!        Bug fix: now a new, empty directory record with valid
!        backward and forward pointers is written immediately
!        when it is created. This prevents an unsegregated file
!        from being left with an invalid forward pointer.
!
!-    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if the DAS open routines ever
!        change.
!
!        Removed an unused variable, PREV.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      INTEGER               NWC
      PARAMETER           ( NWC = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI =  256 )

!
!     Directory pointer locations (backward and forward):
!
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC = 1 )

      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )

!
!     Directory address range locations
!
      INTEGER               CHRRNG
      PARAMETER           ( CHRRNG =          3 )

      INTEGER               DPRNG
      PARAMETER           ( DPRNG  = CHRRNG + 2 )

      INTEGER               INTRNG
      PARAMETER           ( INTRNG = DPRNG  + 2 )

!
!     Location of first type descriptor
!
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )




!
!     Local variables
!
      INTEGER               DESCR
      INTEGER               DIRREC ( NWI )
      INTEGER               DSCREC
      INTEGER               FREE
      INTEGER               I
      INTEGER               LAST
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LREC
      INTEGER               LOC
      INTEGER               LTYPE
      INTEGER               LWORD
      INTEGER               MAXADR
      INTEGER               MINADR
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NEEDED
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NW
      INTEGER               RECNO
      INTEGER               RNGLOC
      INTEGER               ROOM



!
!     Saved variables
!
!

!
!     NEXT maps the DAS data type codes to their successors.
!
      INTEGER               NEXT   ( 3 )
      SAVE                  NEXT

!
!     Initial values
!
      DATA                  NEXT   /  2,   3,   1  /




!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASCUD' )
      END IF

!
!     Here's a preview of coming attractions:
!
!        We're going to update the directories in the indicated
!        DAS file to reflect the addition of NWORDS new data words.
!        This data is supposed to have been added to the file BEFORE
!        this routine is called.  There are several possible states
!        the file can be in at the point this routine is called.
!
!
!           1)  There is already a descriptor of TYPE in the file, and
!               the addition of data does not require this descriptor
!               to be modified.
!
!               We can tell that we have this case when the file
!               summary indicates that, before the addition of data,
!               there was room for NWORDS of data in the last data
!               record in the file.  Since no new data records were
!               required to accommodate the new data, the descriptor
!               for TYPE does not have to be updated.
!
!               However, even though the descriptor need not be
!               modified, the address range for TYPE covered by the
!               directory record containing this last descriptor must be
!               updated, as must be the file summary.
!
!
!           2)  There is already a descriptor of TYPE in the file, and
!               in order to describe the new data added to the file,
!               it suffices to update this descriptor and the address
!               range in the directory containing it.
!
!               This happens when case (1) doesn't apply, and the
!               descriptor of TYPE is the last descriptor in the last
!               directory, and the descriptor is not in the last
!               position (index NWI) of the directory.
!
!               Note that we never update the last descriptor in a
!               directory record.  The reason for this is that after
!               this descriptor is written, we build a new directory
!               record.  All subsequent additions of data are made to
!               records that follow this new directory record;
!               otherwise, the new directory would get overwritten
!               with data.
!
!
!           3)  A new descriptor of TYPE is needed.
!
!               This can happen in several ways:
!
!               a)  There are no directories in the file yet, in which
!                   case space has been reserved for the first
!                   directory.
!
!                   This can happen only when the file had no data at
!                   all in it before the last addition of data.
!
!                   In this case, we must fill in the first descriptor
!                   and the address range for TYPE.  We must also update
!                   the file summary, because the descriptor location,
!                   last logical address of TYPE, and the free pointer
!                   have changed.
!
!               b)  The conditions for cases (1) and (2) are not
!                   satisfied, and the current last directory record
!                   has room for a new descriptor.  In this case, if
!                   the data addition filled in the last data record
!                   described by the current last descriptor of type,
!                   (which will usually be the case), we must update
!                   the appropriate address range in the directory
!                   record containing that descriptor.  We will then
!                   add a new descriptor to the last directory record
!                   and update the address range for TYPE in that
!                   record.  The file summary must be updated as well.
!
!                   If the new descriptor we've added went into the
!                   last slot in a directory record (index NWI), we
!                   also create a new, empty directory record and
!                   update the forward pointer of the current directory
!                   to point to it.  We also update the file summary
!                   so that the free pointer points to the record
!                   following the empty directory record.
!
!
!               c)  The conditions for cases (1) and (2) are not
!                   satisfied, and the current last directory record
!                   has no room for a new descriptor.
!
!                   In this case, if the data addition filled in the
!                   last data record described by the current last
!                   descriptor of TYPE, (which will usually be the
!                   case), we must update the appropriate address range
!                   in the directory record containing that descriptor.
!                   We will then add a new descriptor to the empty
!                   directory record and initialize the address range
!                   for TYPE in that record.  The file summary must be
!                   updated as well.
!
!
!     To start out, we'll need to find out how the file is currently
!     disposed.  We'll need the location of the last descriptor of
!     TYPE, the last logical address of TYPE, and the location of
!     the last descriptor of any type.
!
!     Get the file summary.
!
      CALL DASHFS ( HANDLE, &
                    NRESVR, &
                    NRESVC, &
                    NCOMR, &
                    NCOMC, &
                    FREE, &
                    LASTLA, &
                    LASTRC, &
                    LASTWD )

!
!     Now do all of the data-type-dependent work:
!
!        -- Set the last address of the indicated data type LAST.
!
!        -- Set the physical record of the last descriptor of TYPE.
!
!        -- Set the number of words of data of the specified type per
!           physical record NW.
!
!        -- Set the address range location used to pick address ranges
!           out of directory records.
!
!
!     Note that the address and descriptor location information from
!     the file summary is assumed NOT to take into account the latest
!     data addition.
!
!
      LAST    =  LASTLA( TYPE )
      DSCREC  =  LASTRC( TYPE )

      IF ( TYPE .EQ. DP ) THEN

         NW      =  NWD
         RNGLOC  =  DPRNG

      ELSE IF ( TYPE .EQ. INT ) THEN

         NW      =  NWI
         RNGLOC  =  INTRNG

      ELSE IF ( TYPE .EQ. CHAR ) THEN

         NW      =  NWC
         RNGLOC  =  CHRRNG

      ELSE

         CALL SETMSG ( 'Invalid data type: #. ' )
         CALL ERRINT ( '#',  TYPE               )
         CALL SIGERR ( 'SPICE(DASINVALIDTYPE)'  )
         CALL CHKOUT ( 'DASCUD'                 )
         RETURN

      END IF

!
!     Make sure that NWORDS is something sensible.
!
      IF ( NWORDS .LT. 0 ) THEN

         CALL SETMSG ( 'NWORDS was #; should be non-negative.' )
         CALL ERRINT ( '#',  NWORDS                            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
         CALL CHKOUT ( 'DASCUD'                                )
         RETURN

      END IF

!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file, and also find the type of the descriptor
!     LTYPE.
!
      CALL MAXAI  ( LASTRC,  3,  LREC,  LOC )
      LWORD  =  0
      LTYPE  =  0

      DO I = 1, 3

         IF (       ( LASTRC(I) .EQ. LREC  ) &
              .AND. ( LASTWD(I) .GT. LWORD )  ) THEN

            LWORD = LASTWD(I)
            LTYPE = I

         END IF

      END DO

!
!     LREC, LWORD, and LTYPE are now the record, word, and data type
!     of the last descriptor in the file.  If LREC is zero, there are
!     no directories in the file yet.  In this case, LWORD and
!     LTYPE are both zero.
!

!
!     Compute the number of words we have room for in the current
!     last data record of the indicated type.
!
      IF ( LAST .GT. 0 ) THEN
         ROOM  =  NW   -   (  LAST  -  ( (LAST-1) / NW ) * NW  )
      ELSE
         ROOM  =  0
      END IF

!
!     Compute the number of additional data records needed to
!     accommodate (NWORDS - ROOM) additional words of data of type
!     TYPE.
!
      NEEDED  =  ( NWORDS - ROOM + NW - 1 ) / NW


!
!     Now, update the descriptor directories.
!

      IF (  ( ROOM .GE. NWORDS ) .AND. ( DSCREC .GT. 0 )  ) THEN
!
!        This is case (1).
!
!        There is already a descriptor of TYPE in the file.  The data
!        fits in the current record, so no descriptors have to change.
!
!        Update the address range in the directory record containing
!        the last descriptor of TYPE.
!
         MAXADR  =  LAST + NWORDS

         CALL DASURI ( HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )

!
!        The last logical address of TYPE is now MAXADR.
!
         LASTLA( TYPE )  =  MAXADR

!
!        Write out the updated file summary.
!
         CALL DASUFS ( HANDLE, &
                       NRESVR, &
                       NRESVC, &
                       NCOMR, &
                       NCOMC, &
                       FREE, &
                       LASTLA, &
                       LASTRC, &
                       LASTWD )



      ELSE IF (       ( TYPE   .EQ. LTYPE ) &
                .AND. ( DSCREC .GT. 0     ) &
                .AND. ( LWORD  .LT. NWI   )  ) THEN
!
!
!        This is case (2).
!
!        The descriptor of TYPE is the last descriptor in the
!        file but is not in the last location (index NWI) of a
!        directory record.  All we have to do is update this last
!        descriptor to reflect the addition of the number of needed
!        data records.
!
!        Get the old descriptor, since we're going to update it.
!
!
         CALL DASRRI ( HANDLE, DSCREC, LWORD, LWORD, DESCR )

!
!        Update the descriptor and write it back into the file.
!
         IF ( DESCR .LT. 0 ) THEN
            DESCR = DESCR - NEEDED
         ELSE
            DESCR = DESCR + NEEDED
         END IF

         CALL DASURI ( HANDLE, DSCREC, LWORD, LWORD, DESCR )

!
!        Update the address range for this type.
!
         MAXADR = LAST + NWORDS

         CALL DASURI ( HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )

!
!        The last logical address of TYPE is now MAXADR.  The first
!        free record follows the last data record in use.
!
         LASTLA( TYPE )  =  MAXADR
         FREE            =  FREE  +  NEEDED

!
!        Write out the updated file summary.
!
         CALL DASUFS ( HANDLE, &
                       NRESVR, &
                       NRESVC, &
                       NCOMR, &
                       NCOMC, &
                       FREE, &
                       LASTLA, &
                       LASTRC, &
                       LASTWD )


      ELSE
!
!        This is case (3).  We need a new descriptor.
!

         IF ( LREC .EQ. 0 ) THEN
!
!           This is case (3a).  We have a virgin directory record.
!           Set the number of this record.
!
            RECNO  =  NRESVR + NCOMR + 2

!
!           Start with an empty directory record.
!
            CALL CLEARI ( NWI, DIRREC )

!
!           Add a new descriptor to the directory.  The record
!           count is the number of new records required:  NEEDED.
!
            DIRREC ( BEGDSC     )  =  TYPE
            DIRREC ( BEGDSC + 1 )  =  NEEDED

!
!           Fill in the address range for TYPE covered by this
!           directory.
!
            DIRREC ( RNGLOC     )  =  1
            DIRREC ( RNGLOC + 1 )  =  NWORDS

!
!           Write out this directory.
!
            CALL DASWRI ( HANDLE, RECNO, DIRREC )

!
!           Update the file summary:  the location of the descriptor
!           and the last logical address for this type must be set.
!           The count portion of the descriptor goes after the initial
!           data type indicator; this data type indicator is not
!           considered to be part of the descriptor.
!
!           The first free record follows the last data record in use.
!
            FREE            =  RECNO  + NEEDED + 1
            LASTLA( TYPE )  =  NWORDS
            LASTRC( TYPE )  =  RECNO
            LASTWD( TYPE )  =  BEGDSC + 1

            CALL DASUFS ( HANDLE, &
                          NRESVR, &
                          NRESVC, &
                          NCOMR, &
                          NCOMC, &
                          FREE, &
                          LASTLA, &
                          LASTRC, &
                          LASTWD )


         ELSE IF ( LWORD .LT. NWI ) THEN
!
!           This is case (3b).  We have room for another descriptor
!           in the current directory record.
!
!           Before adding the new descriptor, we must update the
!           directory containing the current last descriptor of TYPE,
!           if the range of addresses covered by the cluster it
!           describes was increased by the last data addition.  Of
!           course, this update is required only if there IS such a
!           descriptor, and if it is in a record that precedes LREC.
!
            IF (       ( DSCREC .GT. 0    ) &
                 .AND. ( DSCREC .LT. LREC ) &
                 .AND. ( ROOM   .GT. 0    )   )  THEN
!
!              Update the address range for TYPE in record DSCREC.
!              The upper bound is increased by ROOM, since that many
!              words of TYPE were added to the last record in the
!              last cluster of TYPE described by that directory.
!
               MAXADR = LAST + ROOM

               CALL DASURI (HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )

            END IF

!
!           Make up the new descriptor and write it to the last
!           directory, following the current last descriptor.  The
!           sign of the new descriptor is a function of the type of
!           the current last descriptor.
!
            IF (  TYPE  .EQ.  NEXT(LTYPE)  ) THEN
!
!              TYPE is the successor in the type sequence of the type
!              of the previous descriptor; use a positive count.
!
               DESCR  =   NEEDED

            ELSE
               DESCR  =  -NEEDED

            END IF

            CALL DASURI ( HANDLE, LREC, LWORD+1,  LWORD+1,  DESCR  )

!
!           Update the address range for this type.  Some care is needed
!           when updating the minimum address:  this value should be
!           assigned only if this is the first descriptor of TYPE in
!           this directory record.
!
            IF ( DSCREC .LT. LREC ) THEN

               MINADR = LAST + ROOM + 1

               CALL DASURI ( HANDLE, LREC, RNGLOC, RNGLOC, MINADR )

            END IF


            MAXADR = LAST + NWORDS

            CALL DASURI ( HANDLE, LREC, RNGLOC+1, RNGLOC+1, MAXADR )

!
!           Update the file summary:  the location of the descriptor
!           and the last logical address for this type must be set.
!
!           The first free record follows the last data record in use.
!
            FREE            =  FREE  +  NEEDED
            LASTLA( TYPE )  =  LAST  +  NWORDS
            LASTRC( TYPE )  =  LREC
            LASTWD( TYPE )  =  LWORD +  1

!
!           Before writing out the summary, see whether we'll need
!           a new directory; this will decide whether the first free
!           record changes.
!
!           If we just filled in the last descriptor in a directory,
!           it's time to add a new directory record to the file.
!           All we have to do at the moment is make room for it, and
!           set the forward pointer of the current directory record
!           to point to the saved record.  Initialize the pointers
!           of the new directory record to make the linked list valid.
!

            IF ( LWORD+1 .EQ. NWI ) THEN
!
!              Update the previous directory to point forward to the
!              next one.
!
               CALL DASURI ( HANDLE, LREC, FWDLOC, FWDLOC, FREE )


!
!              Prepare the new directory record: clear it, set the
!              backward pointer, and write the record.
!
               CALL CLEARI ( NWI, DIRREC )

               DIRREC ( BWDLOC )  =  LREC

               CALL DASWRI ( HANDLE, FREE, DIRREC )

!
!              Update the free record number.
!
               FREE = FREE + 1

            END IF

!
!           Now write out the file summary.
!
            CALL DASUFS ( HANDLE, &
                          NRESVR, &
                          NRESVC, &
                          NCOMR, &
                          NCOMC, &
                          FREE, &
                          LASTLA, &
                          LASTRC, &
                          LASTWD )


         ELSE
!
!           This is case (3c).  We must put the new descriptor in
!           the last directory record, which is currently empty.
!
!           As in case (3b), we may have to update the directory
!           containing the current last descriptor of TYPE, if the
!           range of addresses covered by the cluster it describes was
!           increased by the last data addition.  Of course, this
!           update is required only if there IS such a descriptor.
!
            IF (  ( DSCREC .GT. 0 ) .AND. ( ROOM .GT. 0 )  )  THEN
!
!              Update the address range for TYPE in record DSCREC.
!              The upper bound is increased by ROOM, since that many
!              words of TYPE were added to the last record in the
!              last cluster of TYPE described by that directory.
!
               MAXADR = LAST + ROOM

               CALL DASURI (HANDLE, DSCREC, RNGLOC+1, RNGLOC+1, MAXADR )

            END IF

!
!           Obtain the record number for this directory.
!
            CALL DASRRI ( HANDLE, LREC, FWDLOC, FWDLOC, RECNO )

!
!           Now fill in the new directory record.  Start with a clean
!           record.
!
            CALL CLEARI ( NWI, DIRREC )

!
!           Set the backward pointer, the address range for TYPE,
!           initial data type, and record count.
!
            DIRREC ( BWDLOC     )  =  LREC
            DIRREC ( RNGLOC     )  =  LAST   +  ROOM    +  1
            DIRREC ( RNGLOC + 1 )  =  LAST   +  NWORDS
            DIRREC ( BEGDSC     )  =  TYPE
            DIRREC ( BEGDSC + 1 )  =  NEEDED

!
!           Write out the record.
!
            CALL DASWRI ( HANDLE, RECNO, DIRREC )


!
!           Update the file summary to reflect the new record and word
!           offsets of the last descriptor of the indicated type.  The
!           last address of TYPE has increased also.  The first free
!           record lies after the added data records.
!
            FREE            =  FREE   +  NEEDED
            LASTLA( TYPE )  =  LAST   +  NWORDS
            LASTRC( TYPE )  =  RECNO
            LASTWD( TYPE )  =  BEGDSC +  1

            CALL DASUFS ( HANDLE, &
                          NRESVR, &
                          NRESVC, &
                          NCOMR, &
                          NCOMC, &
                          FREE, &
                          LASTLA, &
                          LASTRC, &
                          LASTWD )

         END IF


      END IF


      CALL CHKOUT ( 'DASCUD' )
      RETURN
      END
