!$Procedure DASACR ( DAS, add comment records )

      subroutine dasacr ( handle, n )

! $ Abstract
!
!     Increase the size of the comment area in a DAS file to accommodate
!     a specified number of additional comment records.
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

      implicit none

      integer               handle
      integer               n

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   A DAS file handle.
!     N          I   Number of comment records to append to the comment
!                    area of the specified file.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an existing DAS file opened for
!              comment area modification by DASOPC.
!
!     N        is the number of records to append to the comment
!              area. If NCOMR is the number of comment records
!              present in the file on input, then on output the
!              number of comment records will be NCOMR + N.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If an I/O error occurs during the addition process, the error
!         is signaled by a routine in the call tree of this routine. The
!         DAS file will probably be corrupted in this case.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is used to create space in the comment area of a DAS
!     file to allow addition of comments to the file. If there are
!     comment records present in the file at the time this routine is
!     called, the number of comment records specified by the input
!     argument N will be appended to the existing comment records.
!     In any case, any existing directory records and data records will
!     be shifted down by N records.
!
!     This routine updates the file record of the specified DAS file
!     to reflect the addition of records to the file's comment area.
!     Also, the file summary obtainable from DASHFS will be updated to
!     reflect the addition of comment records.
!
!     This routine may be used only on existing DAS files opened by
!     DASOPW.
!
!     The association of DAS logical addresses and data within the
!     specified file will remain unaffected by use of this routine.
!
!     Normally, SPICELIB applications will not call this routine
!     directly, but will add comments by calling DASAC.
!
!     This routine has an inverse DASRCR, which removes a specified
!     number of records from the end of the comment area.
!
! $ Examples
!
!     1)  Make room for 10 comment records in the comment area of a
!         new DAS file.
!
!            C
!            C     Create a new DAS file.
!            C
!                  CALL DASOPW ( DAS, HANDLE )
!
!            C
!            C     Now add 10 comment records to the comment area.
!            C
!                  CALL DASACR ( HANDLE, 10 )
!
! $ Restrictions
!
!     1)  The DAS file must have a binary file format native to the host
!         system.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB)
!
!        Updated to support integration with the handle
!        manager subsystem.
!
!        Cleaned up use of unnecessary variables and unneeded
!        declarations.
!
!-    SPICELIB Version 1.1.0, 11-OCT-1996 (NJB)
!
!        Bug fix: backward and forward directory record pointers
!        are now updated when directory records are moved.
!
!-    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     add comment records to a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 11-OCT-1996 (NJB)
!
!        Bug fix: backward and forward directory record pointers
!        are now updated when directory records are moved.
!
!        Because these pointers are not used by the DAS software
!        once a DAS file is segregated, this bug had no effect on
!        DAS files that were created and closed via DASCLS, then
!        commented via the commnt utility.
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return


!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      integer               nwc
      parameter           ( nwc    = 1024 )

      integer               nwd
      parameter           ( nwd    =  128 )

      integer               nwi
      parameter           ( nwi    =  256 )

!
!     Data type parameters
!
      integer               char
      parameter           ( char   =   1  )

      integer               dp
      parameter           ( dp     =   2  )

!
!     Directory pointer locations (backward and forward):
!
      integer               bwdloc
      parameter           ( bwdloc =   1  )

      integer               fwdloc
      parameter           ( fwdloc =   2  )

!
!     Location of first type descriptor
!
      integer               begdsc
      parameter           ( begdsc = 9 )


!
!     Local variables
!
      character*(nwc)       recc

      double precision      recd   ( nwd )

      integer               base
      integer               dirrec ( nwi )
      integer               free
      integer               i
      integer               lastla ( 3 )
      integer               lastrc ( 3 )
      integer               lastwd ( 3 )
      integer               loc
      integer               lrec
      integer               lindex
      integer               lword
      integer               ncomc
      integer               ncomr
      integer               nrec
      integer               nresvc
      integer               nresvr
      integer               nxttyp
      integer               pos
      integer               prec
      integer               reci   ( nwi    )
      integer               type
      integer               unit


!
!     Saved variables
!

!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      integer               next   ( 3 )
      save                  next

      integer               prev   ( 3 )
      save                  prev

!
!     Initial values
!
      data                  next   /  2,   3,   1  /
      data                  prev   /  3,   1,   2  /



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASACR' )

!
!     Programmer's note: the calls to
!
!        DASIOC
!        DASIOD
!        DASIOI
!
!     for read access are valid only for native format DAS files.
!     If this routine is updated to support writing to non-native
!     DAS files, at least the calls to the numeric I/O routines
!     will need to be replaced. (Consider using ZZDASGRD, ZZDASGRI.)
!
!     Make sure this DAS file is open for writing.  Signal an error if
!     not.
!
      call dassih ( handle, 'WRITE' )

!
!     Get the logical unit for this DAS file.
!
      call zzddhhlu ( handle, 'DAS', .false., unit )

      if ( failed() ) then
         call chkout ( 'DASACR' )
         return
      end if

!
!     It's a mistake to use a negative value of N.
!
      if ( n .lt. 0 ) then

         call setmsg ( 'Number of comment records to add must be '    // &
                       'non-negative.  Actual number requested was #.' )
         call errint ( '#',  n                                         )
         call sigerr ( 'SPICE(DASINVALIDCOUNT)'                        )
         call chkout ( 'DASACR'                                        )
         return

      end if

!
!     Before doing anything to the file, make sure that the DASRWR
!     data buffers do not contain any updated records for this file.
!     All of the record numbers that pertain to this file and remain
!     in the DASRWR buffers will be invalidated after this routine
!     returns.
!
!     DASWBR flushes buffered records to the file.
!
      call daswbr ( handle )

!
!     Grab the file summary for this DAS file.  Find the number of
!     comment records and the number of the first free record.
!
      call dashfs ( handle, &
                    nresvr, &
                    nresvc, &
                    ncomr, &
                    ncomc, &
                    free, &
                    lastla, &
                    lastrc, &
                    lastwd )

!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file.
!
      call maxai  ( lastrc,  3,  lrec,  loc )
      lword  =  0

      do i = 1, 3

         if (       ( lastrc(i) .eq. lrec  ) &
              .and. ( lastwd(i) .gt. lword )  ) then

            lword = lastwd(i)

         end if

      end do

!
!     LREC and LWORD are now the record and word index of the last word
!     of the last descriptor in the file. If LREC is zero, there are no
!     directories in the file yet. However, even DAS files that don't
!     contain any data have their first directory records zeroed out,
!     and this should remain true after the addition of the comment
!     records.
!
      if ( lrec .eq. 0 ) then
!
!        Just write the zero-filled record to record number
!
!           NRESVR + NCOMR + N + 2
!
         call cleari ( nwi,                             dirrec )
         call dasioi ( 'WRITE', unit, nresvr+ncomr+n+2, dirrec )


      else
!
!        There really is stuff to move.  For each directory record,
!        move all of the records described by that directory.  We start
!        with the last directory and work our way toward the beginning
!        of the file.
!
         nrec  =  lrec

         do while ( nrec .gt. 0 )
!
!           For each descriptor in the current directory, move the
!           cluster of data records it refers to.
!
!           Read the current directory record.
!
            call dasioi ( 'READ', unit, nrec, dirrec )

!
!           Find the data type, size, and base record number of the
!           last cluster in the current directory.  To do this,
!           traverse the directory record, keeping track of the record
!           count and data types of descriptors as we go.
!
            type  =  dirrec( begdsc )
            base  =  nrec + 1

            if ( nrec .eq. lrec ) then
               lindex = lword
            else
               lindex = nwi
            end if

            do i = begdsc+2, lindex

               if ( dirrec(i) .lt. 0 ) then
                  type = prev(type)
               else
                  type = next(type)
               end if

               base  =  base + abs( dirrec(i-1) )

            end do

!
!           TYPE and BASE are now the data type and base record number
!           of the last cluster described by the current directory.
!
!           We'll now traverse the directory in reverse order, keeping
!           track of cluster sizes and types as we go.
!
!           POS will be the index of the descriptor of the current
!           cluster.
!
            pos  =  lindex

            do while ( pos .gt. begdsc )

               if ( pos .lt. lindex ) then
!
!                 We'll need to determine the type of the current
!                 cluster.  If the next descriptor contains a positive
!                 value, the data type of the cluster it refers to is
!                 the successor of the current type, according to our
!                 ordering of types.
!
                  if (  dirrec(pos+1)  .gt.  0  ) then
!
!                    This assignment and the one below in the ELSE
!                    block are performed from the second loop iteration
!                    onward. NXTTYP is initialized on the first loop
!                    iteration.
!
                     type  =  prev( nxttyp )
                  else
                     type  =  next( nxttyp )
                  end if

!
!                 Update the cluster base record number.
!
                  base    =  base - abs ( dirrec(pos) )

               end if

!
!              Move the current cluster.
!
               do i  =  base + abs( dirrec(pos) ) - 1,    base,    -1

                  if ( type .eq. char ) then

                     call dasioc ( 'READ',   unit,  i,    recc )
                     call dasioc ( 'WRITE',  unit,  i+n,  recc )

                  else if ( type .eq. dp ) then

                     call dasiod ( 'READ',   unit,  i,    recd )
                     call dasiod ( 'WRITE',  unit,  i+n,  recd )

                  else

                     call dasioi ( 'READ',   unit,  i,    reci )
                     call dasioi ( 'WRITE',  unit,  i+n,  reci )

                  end if

               end do

!
!              The next descriptor to look at is the preceding one in
!              the directory.
!
               pos     =  pos - 1
               nxttyp  =  type

            end do

!
!           Find the preceding directory record.
!
            prec = dirrec ( bwdloc )

!
!           Update the backward and forward pointers in the current
!           directory record.  However, don't modify null pointers.
!
            if ( dirrec(fwdloc) .gt. 0 ) then
               dirrec(fwdloc)  =  dirrec(fwdloc) + n
            end if

            if ( dirrec(bwdloc) .gt. 0 ) then
               dirrec(bwdloc)  =  dirrec(bwdloc) + n
            end if

!
!           Move the current directory record.
!
            call dasioi ( 'WRITE',  unit,  nrec+n,  dirrec )

!
!           Consider the previous directory.
!
            nrec = prec

         end do

      end if

!
!     Update the file summary.  The number of comment records and the
!     number of the first free record have been incremented by N.
!     The numbers of the records containing the last descriptor of each
!     type have been incremented by N only if they were non-zero.
!
!     The call to DASUFS will update the file record as well as the
!     file summary.
!
      ncomr  =  ncomr + n
      free   =  free  + n

      do i = 1, 3

         if ( lastrc(i) .ne. 0 ) then
            lastrc(i)  =  lastrc(i) + n
         end if

      end do


      call dasufs ( handle, &
                    nresvr, &
                    nresvc, &
                    ncomr, &
                    ncomc, &
                    free, &
                    lastla, &
                    lastrc, &
                    lastwd )

      call chkout ( 'DASACR' )
      return
      end
