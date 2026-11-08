!$Procedure      ZZEKSINF ( EK, return segment information )

      subroutine zzeksinf ( handle,  segno,   tabnam, &
                            segdsc,  cnames,  cdscrs )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return general segment information for a specified segment in a
!     specified EK.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations

      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ektnamsz.inc'

      integer(ip)           handle
      integer(ip)           segno
      character*(*)         tabnam
      integer(ip)           segdsc ( sdscsz    )
      character*(*)         cnames (         * )
      integer(ip)           cdscrs ( cdscsz, * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of EK.
!     SEGNO      I   Number of segment to be summarized.
!     TABNAM     O   Name of table containing segment.
!     SEGDSC     O   Segment descriptor.
!     CNAMES     O   Names of columns in segment.
!     CDSCRS     O   Descriptors of columns in segment.
!
! $ Detailed_Input
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGNO          is the number of the segment whose summary is
!                    desired.  Segments are numbered from 1 to NSEG,
!                    where NSEG is the count of segments in the file.
!
! $ Detailed_Output
!
!     TABNAM         is the name of the table to which the segment
!                    belongs.
!
!     SEGDSC         is an EK segment descriptor.  The contents of this
!                    integer array are described in the include file
!
!                       eksegdsc.inc.
!
!                    Two commonly used elements of the
!                    descriptor are the number of rows in the table
!                    and the number of columns in the table.  The
!                    indices of these items are given by the parameters
!                    NRIDX and NCIDX, respectively.
!
!
!     CNAMES         is a list of names of data columns in the segment.
!
!     CDSCRS         is a list of descriptors of columns in the segment.
!                    Elements (1:CDSCSZ,I) of this integer array
!                    comprise the descriptor of the Ith column in the
!                    segment.  The contents of a column descriptor are
!                    listed below.  The parameters shown in the first
!                    subscript of CDSCRS are declared in the include
!                    file
!
!                       ekcoldsc.inc.
!
!                    We recommend using these parameters in any calling
!                    routine.
!
!                       CDSCRS(CLSIDX,I):    Column class
!                       CDSCRS(TYPIDX,I):    Data type
!                       CDSCRS(LENIDX,I):    String length
!                       CDSCRS(SIZIDX,I):    Element size
!                       CDSCRS(NAMIDX,I):    Column name base address
!                       CDSCRS(IXTIDX,I):    Column index's type code
!                       CDSCRS(IXPIDX,I):    Column index's pointer
!                       CDSCRS(NULIDX,I):    Null flag
!                       CDSCRS(ORDIDX,I):    Column's ordinal position
!                                            in parent table
!                       CDSCRS(METIDX,I):    Column's integer metadata
!                                            pointer
!                       CDSCRS(11,I):        Reserved.
!
!                    Notes:
!
!                       1) Element 3 applies only to character columns.
!
!                          Element 3 takes the boolean value IFALSE
!                          if the column contains variable-length
!                          strings.
!
!                          The boolean parameter IFALSE is represented
!                          by the integer -1.
!
!                       2) Element 4 takes the boolean value IFALSE
!                          if the column contains variable-size
!                          arrays.
!
!                       3) Element 6 takes the value IFALSE if the
!                          column is not indexed.
!
!                       4) Element 8 takes the value IFALSE if null
!                          values are not allowed in the column.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The output arguments will not be
!         modified.
!
!     2)  If SEGNO is not the index of an existing segment in the
!         specified file, the error SPICE(INDEXOUTOFRANGE) will be
!         signaled.  The output arguments will not be modified.
!
!     3)  If an I/O error occurs while attempting to obtain summary
!         information for the specified segment, the error will be
!         diagnosed by routines called by this routine.  The output
!         arguments may be modified in this case.
!
! $ Files
!
!     See the description of HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine supports the function of summarizing a binary
!     EK file, allowing NAIF Toolkit users to determine whether it
!     contains data of interest.  The routine also also provides
!     address information necessary to retrieve information from the
!     segment.
!
! $ Examples
!
!     1)  Dump the table and column names of the segments in an EK.
!
!            C
!            C     Open the EK for read access and get the number of
!            C     segments it
!            C     contains.
!            C
!                  CALL EKOPR ( EKNAME, HANDLE )
!
!                  NSEG = EKNSEG ( HANDLE )
!
!            C
!            C     Loop through the segments, dumping the desired
!            C     summary information for each one.
!            C
!                  WRITE (*,*) ' '
!                  WRITE (*,*) ' '
!                  WRITE (*,*) 'Segment summary for file ', EKNAME
!                  WRITE (*,*) ' '
!                  WRITE (*,*) ' '
!
!                  DO I = 1, NSEG
!
!                     CALL ZZEKSINF ( HANDLE,  I,      TABNAM,
!                 .                   SEGDSC,  CNAMES, CDSCRS  )
!
!                     WRITE (*,*)
!                 .   '========================================'      //
!                 .   '========================================'
!
!
!                     WRITE (*,*) 'Table containing segment: ', TABNAM
!
!                     WRITE (*,*) ' '
!                     WRITE (*,*) 'Column names: '
!                     WRITE (*,*) ' '
!
!                     DO J = 1, SEGDSC(NCIDX)
!
!                        WRITE (*,*) '   '//CNAMES(J)
!
!                     END DO
!
!                     WRITE (*,*)
!                 .   '========================================'      //
!                 .   '========================================'
!
!                  END DO
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
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.1.0, 03-JUL-1996 (NJB)
!
!        Bug fix:  table and column names are now padded with trailing
!        blanks on output if necessary.
!
!-    Beta Version 1.0.0, 27-SEP-1995 (NJB)
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 03-JUL-1996 (NJB)
!
!        Bug fix:  table and column names are now padded with trailing
!        blanks on output if necessary.  Previously, if the caller
!        declared these variables with string lengths longer than
!        TNAMSZ and CNAMSZ respectively, the trailing characters
!        at positions past those designated by these parameters were
!        left unassigned on output.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           eknseg

      logical               failed
      logical               return

!
!     Local variables
!
      integer(ip)           base
      integer(ip)           i
      integer(ip)           ncols
      integer(ip)           nseg
      integer(ip)           p


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKSINF' )
      end if

!
!     Verify that the target file is a paged DAS EK open for read
!     access, or we can't summarize the file.
!
      call zzekpgch ( handle, 'READ' )

      if ( failed() ) then
         call chkout ( 'ZZEKSINF' )
         return
      end if

!
!     Find out how many segments are in the file, so we can check
!     the index for validity.
!
      nseg = eknseg ( handle )

      if (  ( segno .lt. 1 ) .or. ( segno .gt. nseg )  ) then

         call setmsg ( 'Segment index was #; valid range is 1:#' )
         call errint ( '#',  segno                               )
         call errint ( '#',  nseg                                )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'                  )
         call chkout ( 'ZZEKSINF'                                )
         return

      end if

!
!     We're ready to proceed.  The first step is to find the
!     segment's metadata location and look up the segment descriptor.
!
      call zzekmloc ( handle, segno,  p,           base   )
      call dasrdi   ( handle, base+1, base+sdscsz, segdsc )

!
!     Get the table name.  The table's base address is in the segment
!     descriptor.
!
      call dasrdc ( handle, &
                    segdsc(tnmidx) + 1, &
                    segdsc(tnmidx) + tnamsz, &
                    1, &
                    tnamsz, &
                    tabnam                   )


      if (  len( tabnam )  .gt.  tnamsz  ) then
         tabnam (tnamsz+1:) = ' '
      end if


!
!     Read the column descriptors.  The first one starts at DAS
!     integer address
!
!        BASE + CDOFF + 1.
!
!
      ncols   =  segdsc(ncidx)

      call dasrdi ( handle, &
                    base + cdoff + 1, &
                    base + cdoff + ncols*cdscsz, &
                    cdscrs                        )

!
!     Now read the column names into the names array.
!
      call dasrdc ( handle, &
                    segdsc(nmlidx) + 1, &
                    segdsc(nmlidx) + ncols*cnamsz, &
                    1, &
                    cnamsz, &
                    cnames  )


      if (   len( cnames(1) )  .gt.  cnamsz  ) then

         do i = 1, ncols
            cnames(i)(cnamsz+1:) = ' '
         end do

      end if

!
!     All output arguments are set, or else FAILED() is .TRUE.
!

      call chkout ( 'ZZEKSINF' )
      return
      end
