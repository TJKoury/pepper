!$Procedure DASRCR ( DAS, remove comment records )

      subroutine dasrcr ( handle, n )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Decrease the size of the comment area in a DAS file to reclaim
!     space freed by the removal of a specified number of comment
!     records.
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

      integer(ip)           handle
      integer(ip)           n

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   A DAS file handle.
!     N          I   Number of comment records to remove.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an existing DAS file opened for
!              comment area modification by DASOPC.
!
!     N        is the number of records to remove from the end of
!              the comment area. of the specified file. If NCOMR
!              is the number of comment records present in the
!              file on input, then on output the number of comment
!              records will be MAX ( 0,  NCOMR - N ).
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
!     2)  If an I/O error occurs during the removal process, the error
!         is signaled by a routine in the call tree of this routine. The
!         DAS file will probably be corrupted in this case.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is used to reclaim freed space in the comment area
!     of a DAS file subsequent to removal of comments from the file.
!     Any existing directory records and data records will be shifted
!     up by N records.
!
!     This routine updates the file record of the specified DAS file
!     to reflect the addition of records to the file's comment area.
!     Also, the file summary obtainable from DASHFS will be updated to
!     reflect the addition of comment records.
!
!     The disk space occupied by the specified DAS file will not
!     decrease as a result of calling this routine, but the number of
!     records occupied by meaningful data will decrease. The useful
!     records in the file can be copied by DAS routines to create a
!     new, smaller file which contains only the meaningful data.
!
!     This routine may be used only on existing DAS files opened by
!     DASOPC.
!
!     The association of DAS logical addresses and data within the
!     specified file will remain unaffected by use of this routine.
!
!     Normally, SPICELIB applications will not call this routine
!     directly, but will remove comments by calling DASRC.
!
!     This routine has an inverse DASACR, which appends a specified
!     number of records to the end of the comment area.
!
! $ Examples
!
!     C
!     C     Open an existing DAS file for modification of
!     C     the comment area. We'll presume that the file
!     C     contains 20 comment records.
!     C
!           CALL DASOPC ( DAS, HANDLE )
!
!     C
!     C     Remove the last 10 comment records from the file.
!     C
!           CALL DASRCR ( HANDLE, 10  )
!
!     C
!     C     Close the file.
!     C
!           CALL DASCLS ( HANDLE )
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
!-    SPICELIB Version 1.3.0, 02-JUN-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     remove comment records from a DAS file
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
      integer(ip)           nwc
      parameter           ( nwc    = 1024 )

      integer(ip)           nwd
      parameter           ( nwd    =  128 )

      integer(ip)           nwi
      parameter           ( nwi    =  256 )

!
!     Data type parameters
!
      integer(ip)           char
      parameter           ( char   =   1  )

      integer(ip)           dp
      parameter           ( dp     =   2  )

!
!     Directory pointer location (forward):
!
      integer(ip)           fwdloc
      parameter           ( fwdloc =   1  )

!
!     Location of first type descriptor
!
      integer(ip)           begdsc
      parameter           ( begdsc = 9 )


!
!     Local variables
!
      character(nwc)        recc

      real(wp)              recd   ( nwd )

      integer(ip)           base
      integer(ip)           dirrec ( nwi )
      integer(ip)           free
      integer(ip)           i
      integer(ip)           lastla ( 3 )
      integer(ip)           lastrc ( 3 )
      integer(ip)           lastwd ( 3 )
      integer(ip)           loc
      integer(ip)           lrec
      integer(ip)           lindex
      integer(ip)           lword
      integer(ip)           ncomc
      integer(ip)           ncomr
      integer(ip)           nrec
      integer(ip)           nresvc
      integer(ip)           nresvr
      integer(ip)           nshift
      integer(ip)           pos
      integer(ip)           reci   ( nwi    )
      integer(ip)           type
      integer(ip)           unit


!
!     Saved variables
!

!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      integer(ip)           next   ( 3 )
      save                  next

      integer(ip)           prev   ( 3 )
      save                  prev

!
!     Initial values
!
      data                  next   /  2,   3,   1  /
      data                  prev   /  3,   1,   2  /



!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Make sure this DAS file is open for writing.  Signal an error if
!     not.
!
      call dassih ( handle, 'WRITE' )

!
!     Get the logical unit for this DAS file.
!
      call zzddhhlu ( handle, 'DAS', .false., unit )

      if ( failed() ) return

!
!     It's a mistake to use a negative value of N.
!
      if ( n < 0 ) then

         call setmsg ( 'Number of comment records to remove must be ' // &
                       'non-negative.  Actual number requested was #.' )
         call errint ( '#',  n                                         )
         call sigerr ( 'SPICE(DASINVALIDCOUNT)'                        )
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
!     reserved records and the number of the first free record.
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
!     Determine the size of the record shift we'll actually perform.
!
      nshift  =  min ( n, ncomr )

!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file.
!
      call maxai  ( lastrc,  3,  lrec,  loc )
      lword  =  0

      do i = 1, 3

         if (       ( lastrc(i) == lrec  ) &
              .and. ( lastwd(i) > lword )  ) then

            lword = lastwd(i)

         end if

      end do

!
!     LREC and LWORD are now the record and word index of the last
!     descriptor in the file. If LREC is zero, there are no directories
!     in the file yet. However, even DAS files that don't contain any
!     data have their first directory records zeroed out, and this
!     should remain true after the removal of the comment records.
!
      if ( lrec == 0 ) then
!
!        Just write the zero-filled record to record number
!
!           NRESVR + NCOMR + 2 - NSHIFT
!
         call cleari (  nwi,                                 dirrec )
         call dasioi ( 'WRITE', unit, nresvr+ncomr+2-nshift, dirrec )

      else
!
!        There really is stuff to move.  For each directory record,
!        move the record and then all of the records described by that
!        record.  We start at the beginning of the data area and move
!        downwards in the file as we go.
!
         nrec  =  nresvr + ncomr + 2

         do while ( ( nrec <= lrec ) .and. ( nrec /= 0 ) )
!
!           Read the current directory record and move it.
!
            call dasioi ( 'READ',   unit,  nrec,         dirrec )
            call dasioi ( 'WRITE',  unit,  nrec-nshift,  dirrec )

!
!           For each descriptor in the current directory, move the
!           cluster of data records it refers to.
!
!           Find the data type, size, and base record number of the
!           first cluster described by the current directory.  Also
!           find the index within the directory of the directory's
!           last descriptor.
!
            type  =  dirrec( begdsc )
            base  =  nrec + 1

            if ( nrec == lrec ) then
               lindex = lword
            else
               lindex = nwi
            end if

!
!           We'll now traverse the directory in forward order, keeping
!           track of cluster sizes and types as we go.
!
!           POS will be the index of the descriptor of the current
!           cluster.
!
            pos  =  begdsc + 1

            do while ( pos <= lindex )

               if ( pos > begdsc+1 ) then
!
!                 We'll need to determine the type of the current
!                 cluster.  If the descriptor contains a positive
!                 value, the data type of the cluster it refers to is
!                 the successor of the previous type, according to our
!                 ordering of types.
!
                  if (  dirrec(pos)  >  0  ) then
                     type  =  next( type )
                  else
                     type  =  prev( type )
                  end if

!
!                 Update the cluster base record number.
!
                  base  =  base + abs ( dirrec(pos-1) )

               end if

!
!              BASE and TYPE now are correctly set for the current
!              cluster.  Move the cluster.
!
               do i  =  base,   base + abs( dirrec(pos) ) - 1

                  if ( type == char ) then

                     call dasioc ( 'READ',   unit,  i,         recc )
                     call dasioc ( 'WRITE',  unit,  i-nshift,  recc )

                  else if ( type == dp ) then

                     call dasiod ( 'READ',   unit,  i,         recd )
                     call dasiod ( 'WRITE',  unit,  i-nshift,  recd )

                  else

                     call dasioi ( 'READ',   unit,  i,         reci )
                     call dasioi ( 'WRITE',  unit,  i-nshift,  reci )

                  end if

               end do

!
!              The next descriptor to look at is the next one in the
!              current directory.
!
               pos     =  pos + 1

            end do

!
!           Find the next directory record.
!
            nrec = dirrec ( fwdloc )

         end do

      end if

!
!     Update the file summary.  The number of comment records and the
!     number of the first free record have been decremented by NSHIFT.
!     The numbers of the records containing the last descriptor of each
!     type have been decremented by NSHIFT only if they were non-zero.
!
!
!     The call to DASUFS will update the file record as well as the
!     file summary.
!
      ncomr  =  ncomr - nshift
      free   =  free  - nshift

      do i = 1, 3

         if ( lastrc(i) /= 0 ) then
            lastrc(i)  =  lastrc(i) - nshift
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

      return
      end
