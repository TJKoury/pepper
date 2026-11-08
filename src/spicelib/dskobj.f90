!$Procedure DSKOBJ ( DSK, get object IDs )

      subroutine dskobj ( dskfnm, bodids )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the set of body ID codes of all objects for which
!     topographic data are provided in a specified DSK file.
!
! $ Required_Reading
!
!     CELLS
!     DAS
!     DSK
!     SETS
!     NAIF_IDS
!
! $ Keywords
!
!     COVERAGE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'dla.inc'
      include 'dskdsc.inc'

      integer(ip), parameter :: lbcell = -5

      character(*)          dskfnm
      integer(ip)           bodids ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DSKFNM     I   Name of DSK file.
!     BODIDS    I-O  Set of ID codes of objects in DSK file.
!
! $ Detailed_Input
!
!     DSKFNM   is the name of a DSK file. This file will be opened for
!              read access by this routine.
!
!     BODIDS   is an initialized SPICE set data structure.
!
!              BODIDS optionally may contain a set of body ID codes on
!              input; on output, the data already present in BODIDS will
!              be combined with ID code set found for the file DSKFNM.
!
!              If BODIDS contains no data on input, its size and
!              cardinality still must be initialized.
!
! $ Detailed_Output
!
!     BODIDS   is a SPICE set data structure that contains the union
!              of its contents upon input with the set of body ID codes
!              of segments in the indicated DSK file.
!
!              The elements of SPICE sets are unique; each ID code in
!              BODIDS appears only once, even if the DSK file contains
!              multiple segments for that ID code.
!
!              See the $Examples section below for a complete example
!              program showing how to retrieve the body and surface ID
!              codes from a DSK file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file has transfer format, the error
!         SPICE(INVALIDFORMAT) is signaled.
!
!     2)  If the input file is not a transfer file but has architecture
!         other than DAS, the error SPICE(INVALIDARCHTYPE) is signaled.
!
!     3)  If the input file is a binary DAS file of type other than DSK,
!         the error SPICE(INVALIDFILETYPE) is signaled.
!
!     4)  If the DSK file cannot be opened or read, an error is signaled
!         by a routine in the call tree of this routine.
!
!     5)  If the size of the output set argument BODIDS is insufficient
!         to contain the actual number of ID codes of objects covered by
!         the indicated DSK file, the error SPICE(CELLTOOSMALL) is
!         signaled.
!
! $ Files
!
!     See the description of the argument DSKFNM above.
!
! $ Particulars
!
!     This routine provides an API via which applications can determine
!     the set of objects for which there are topographic data in a
!     specified DSK file.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Display the coverage for each object in a specified DSK file.
!        Find the set of objects in the file. Loop over the contents
!        of the ID code set: find the surface ID for each item in the
!        set and display the surface ID.
!
!        Example code begins here.
!
!        C
!        C     Examine a DSK file and identify the set of
!        C     central bodies associated with the segments
!        C     in the file. For each body, find the
!        C     set of surfaces associated with that body.
!        C
!              PROGRAM DSKOBJ_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               CARDI
!        C
!        C     Local parameters
!        C
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               MAXID
!              PARAMETER           ( MAXID  = 10000 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    DSKFNM
!
!              INTEGER               BODIDS ( LBCELL : MAXID )
!              INTEGER               I
!              INTEGER               J
!              INTEGER               SRFIDS ( LBCELL : MAXID )
!
!        C
!        C     Initialize body ID and surface ID cells.
!        C
!              CALL SSIZEI ( MAXID, BODIDS )
!              CALL SSIZEI ( MAXID, SRFIDS )
!
!        C
!        C     Prompt for the name of a DSK file.
!        C
!              CALL PROMPT ( 'Enter name of DSK file > ', DSKFNM )
!
!        C
!        C     Obtain body ID set for the DSK.
!        C
!              CALL DSKOBJ ( DSKFNM, BODIDS )
!
!              DO I = 1, CARDI( BODIDS )
!
!                 WRITE (*,*) ' '
!                 WRITE (*,*) 'Body ID:     ', BODIDS(I)
!        C
!        C        Get the surface IDs for the Ith body.
!        C
!                 CALL DSKSRF ( DSKFNM, BODIDS(I), SRFIDS )
!
!                 DO J = 1, CARDI( SRFIDS )
!                    WRITE (*,*) '   Surface ID: ', SRFIDS(J)
!                 END DO
!
!              END DO
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
!        Enter name of DSK file > phobos512.bds
!
!         Body ID:              401
!            Surface ID:          401
!
! $ Restrictions
!
!     1)  If an error occurs while this routine is updating the set
!         BODIDS, the set may be corrupted.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-OCT-2021 (JDR) (NJB)
!
!        Changed input argument name "DSK" to "DSKFNM" for consistency
!        with other routines.
!
!        Bug fix: added call to FAILED after call to GETFAT.
!
!        Edited the header comments to comply with NAIF standard.
!        Changed the DKS file used in the code example's solution.
!        Added NAIF_IDS to the $Required_Reading section.
!
!        Corrected short error message in entries #2, #3 and #5 in
!        $Exceptions section.
!
!-    SPICELIB Version 1.0.0, 22-AUG-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     find id codes of ephemeris objects in DSK file
!     find id codes of bodies in DSK file
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           cardi
      integer(ip)           sizei

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: toklen = 4

!
!     Local variables
!
      character(toklen)     arch
      character(toklen)     kertyp

      real(wp)              dskdsc ( dskdsz )

      integer(ip)           bid
      integer(ip)           dladsc ( dladsz )
      integer(ip)           handle
      integer(ip)           nxtdsc ( dladsz )

      logical               found

      if ( return() ) return

!
!     See whether GETFAT thinks we've got a DSK file.
!
      call getfat ( dskfnm, arch, kertyp )

      if ( failed() ) return

      if ( arch == 'XFR' ) then

         call setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If the input file is an ' &
         //            'DSK file in transfer format, run TOBIN on ' &
         //            'the file to convert it to binary format.'  )
         call errch  ( '#',  dskfnm                                )
         call errch  ( '#',  arch                                  )
         call sigerr ( 'SPICE(INVALIDFORMAT)'                      )
         return

      else if ( arch /= 'DAS' ) then

         call setmsg ( 'Input file # has architecture #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. Binary DSK files have ' &
         //            'DAS architecture. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call errch  ( '#',  dskfnm                                )
         call errch  ( '#',  arch                                  )
         call sigerr ( 'SPICE(INVALIDARCHTYPE)'                    )
         return

      else if ( kertyp /= 'DSK' ) then

         call setmsg ( 'Input file # has file type #. The file ' &
         //            'must be a binary DSK file to be readable ' &
         //            'by this routine. If you expected the ' &
         //            'file to be a binary DSK file, the problem ' &
         //            'may be due to the file being an old ' &
         //            'non-native file lacking binary file format ' &
         //            'information. It''s also possible the file ' &
         //            'has been corrupted.'                       )
         call errch  ( '#',  dskfnm                                )
         call errch  ( '#',  kertyp                                )
         call sigerr ( 'SPICE(INVALIDFILETYPE)'                    )
         return

      end if

!
!     Open the DSK for read access; start a forward search.
!
      call dasopr ( dskfnm, handle )
      call dlabfs ( handle, nxtdsc, found )

      if ( failed() ) return

      do while (  found  .and.  ( .not. failed() )  )
!
!        Get the DSK descriptor of the current segment.
!        This is where we'll find the body ID code.
!
         call movei ( nxtdsc, dladsz, dladsc )

         call dskgd ( handle, dladsc, dskdsc )
!
!        The body ID is at location CTRIDX ("center index")
!        of the DSK descriptor.
!
!        Append, rather than insert, the new ID. We'll
!        turn the cell into a set at the end of the loop.
!
!        Before appending, make sure there's room in the cell
!        for another entry. We can't afford to let APPNDI
!        catch an out-of-room error, because we would lose
!        the ability to close the file.
!
         bid = nint( dskdsc(ctridx) )

         if ( cardi(bodids) == sizei(bodids) ) then
!
!           We're going to signal an error. Close the DSK
!           first.
!
            call dskcls ( handle, .false. )

            call setmsg ( 'Cannot append body ID # to cell ' &
            //            'while reading DSK file #. ' &
            //            'Cell size is #.'                 )
            call errint ( '#', bid                          )
            call errch  ( '#', dskfnm                       )
            call errint ( '#', sizei( bodids )              )
            call sigerr ( 'SPICE(CELLTOOSMALL)'             )
            return

         end if

         call appndi ( bid, bodids )
!
!        Fetch the DLA descriptor of the next segment.
!
         call dlafns ( handle, dladsc, nxtdsc, found )

      end do

      call validi ( sizei(bodids), cardi(bodids), bodids )

      call dascls ( handle )

      return
      end
