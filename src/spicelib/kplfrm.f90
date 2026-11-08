!$Procedure KPLFRM ( Kernel pool frame IDs )

      subroutine kplfrm ( frmcls, idset )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return a SPICE set containing the frame IDs of all reference
!     frames of a given class having specifications in the kernel pool.
!
! $ Required_Reading
!
!     CELLS
!     FRAMES
!     KERNEL
!     NAIF_IDS
!     SETS
!
! $ Keywords
!
!     FRAME
!     SET
!     UTILITY
!
! $ Declarations

      implicit none

      include 'frmtyp.inc'
      include 'ninert.inc'
      include 'nninrt.inc'

      integer(ip), parameter :: lbcell = -5

      integer(ip)           frmcls
      integer(ip)           idset  ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRMCLS     I   Frame class.
!     IDSET      O   Set of ID codes of frames of the specified class.
!
! $ Detailed_Input
!
!     FRMCLS   is an integer code specifying the frame class or
!              classes for which frame ID codes are requested.
!              The applicable reference frames are those having
!              specifications present in the kernel pool.
!
!              FRMCLS may designate a single class or "all
!              classes."
!
!              The include file frmtyp.inc declares parameters
!              identifying frame classes. The supported values
!              and corresponding meanings of FRMCLS are
!
!                 Parameter      Value    Meaning
!                 =========      =====    =================
!                 ALL              -1     All frame classes
!                                         specified in the
!                                         kernel pool. Class 1
!                                         is not included.
!
!                 INERTL            1     Built-in inertial.
!                                         No frames will be
!                                         returned in the
!                                         output set.
!
!                 PCK               2     PCK-based frame
!
!                 CK                3     CK-based frame
!
!                 TK                4     Fixed rotational
!                                         offset ("text
!                                         kernel") frame
!
!                 DYN               5     Dynamic frame
!
!                 SWTCH             6     Switch frame
!
! $ Detailed_Output
!
!     IDSET    is a SPICE set containing the ID codes of all
!              reference frames having specifications present in
!              the kernel pool and belonging to the specified
!              class or classes.
!
!              Note that if FRMCLS is set to INERTL, IDSET
!              will be empty on output.
!
! $ Parameters
!
!     See the INCLUDE file frmtyp.inc.
!
! $ Exceptions
!
!     1)  If the input frame class argument is not defined in
!         frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled.
!
!     2)  If the size of IDSET is too small to hold the requested frame
!         ID set, the error SPICE(SETTOOSMALL) is signaled.
!
!     3)  Frames of class 1 may not be specified in the kernel pool.
!         However, for the convenience of users, this routine does not
!         signal an error if the input class is set to INERTL. In this
!         case the output set will be empty.
!
!     4)  This routine relies on the presence of just three kernel
!         variable assignments for a reference frame in order to
!         determine that that reference frame has been specified:
!
!            FRAME_<frame name>       = <ID code>
!            FRAME_<ID code>_NAME     = <frame name>
!
!         and either
!
!            FRAME_<ID code>_CLASS    = <class>
!
!         or
!
!            FRAME_<frame name>_CLASS = <class>
!
!         It is possible for the presence of an incomplete frame
!         specification to trick this routine into incorrectly
!         deciding that a frame has been specified. This routine
!         does not attempt to diagnose this problem.
!
! $ Files
!
!     Reference frame specifications for frames that are not built in
!     are typically established by loading frame kernels.
!
! $ Particulars
!
!     This routine enables SPICE-based applications to conveniently
!     find the frame ID codes of reference frames having specifications
!     present in the kernel pool. Such frame specifications are
!     introduced into the kernel pool either by loading frame kernels
!     or by means of calls to the kernel pool "put" API routines
!
!        PCPOOL
!        PDPOOL
!        PIPOOL
!
!     Given a reference frame's ID code, other attributes of the
!     frame can be obtained via calls to the SPICELIB routines
!
!        FRMNAM {Return a frame's name}
!        FRINFO {Return a frame's center, class, and class ID}
!
!     This routine has a counterpart
!
!        BLTFRM
!
!     which fetches the frame IDs of all built-in reference frames.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Display the IDs and names of all reference frames having
!        specifications present in the kernel pool. Group the outputs
!        by frame class. Also fetch and display the entire set of IDs
!        and names using the parameter ALL.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!           KPL/MK
!
!           File: kplfrm_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name            Contents
!              --------------       --------------------------
!              clem_v20.tf          Clementine FK
!              moon_060721.tf       Generic Lunar SPICE frames
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'clem_v20.tf'
!                                  'moon_060721.tf' )
!           \begintext
!
!           End of meta-kernel
!
!        Example code begins here.
!
!              PROGRAM KPLFRM_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'frmtyp.inc'
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               CARDI
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'kplfrm_ex1.tm' )
!
!              INTEGER               NFRAME
!              PARAMETER           ( NFRAME = 1000 )
!
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 80 )
!
!              INTEGER               FRNMLN
!              PARAMETER           ( FRNMLN = 32 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FRNMLN)    FRNAME
!              CHARACTER*(LNSIZE)    OUTLIN
!
!              INTEGER               I
!              INTEGER               IDSET ( LBCELL : NFRAME )
!              INTEGER               J
!
!        C
!        C     Initialize the frame set.
!        C
!              CALL SSIZEI ( NFRAME, IDSET )
!
!        C
!        C     Load kernels that contain frame specifications.
!        C
!              CALL FURNSH ( META )
!
!        C
!        C     Fetch and display the frames of each class.
!        C
!              DO I = 1, 7
!
!                 IF ( I .LT. 7 ) THEN
!        C
!        C           Fetch the frames of class I.
!        C
!                    CALL KPLFRM ( I, IDSET )
!
!                    OUTLIN = 'Number of frames of class #: #'
!                    CALL REPMI ( OUTLIN, '#', I,            OUTLIN )
!                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
!
!                 ELSE
!        C
!        C           Fetch IDs of all frames specified in the kernel
!        C           pool.
!        C
!                    CALL KPLFRM ( ALL, IDSET )
!
!                    OUTLIN = 'Number of frames in the kernel pool: #'
!                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN )
!
!                 END IF
!
!                 CALL TOSTDO ( ' '    )
!                 CALL TOSTDO ( OUTLIN )
!                 CALL TOSTDO ( '   Frame IDs and names' )
!
!                 DO J = 1, CARDI(IDSET)
!                    CALL FRMNAM ( IDSET(J), FRNAME )
!                    WRITE (*,*) IDSET(J), '  ', FRNAME
!                 END DO
!
!              END DO
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!        Number of frames of class 1: 0
!           Frame IDs and names
!
!        Number of frames of class 2: 1
!           Frame IDs and names
!               31002   MOON_PA_DE403
!
!        Number of frames of class 3: 1
!           Frame IDs and names
!              -40000   CLEM_SC_BUS
!
!        Number of frames of class 4: 11
!           Frame IDs and names
!              -40008   CLEM_CPT
!              -40007   CLEM_BSTAR
!              -40006   CLEM_ASTAR
!              -40005   CLEM_LIDAR
!              -40004   CLEM_LWIR
!              -40003   CLEM_NIR
!              -40002   CLEM_UVVIS
!              -40001   CLEM_HIRES
!               31000   MOON_PA
!               31001   MOON_ME
!               31003   MOON_ME_DE403
!
!        Number of frames of class 5: 0
!           Frame IDs and names
!
!        Number of frames of class 6: 0
!           Frame IDs and names
!
!        Number of frames in the kernel pool: 13
!           Frame IDs and names
!              -40008   CLEM_CPT
!              -40007   CLEM_BSTAR
!              -40006   CLEM_ASTAR
!              -40005   CLEM_LIDAR
!              -40004   CLEM_LWIR
!              -40003   CLEM_NIR
!              -40002   CLEM_UVVIS
!              -40001   CLEM_HIRES
!              -40000   CLEM_SC_BUS
!               31000   MOON_PA
!               31001   MOON_ME
!               31002   MOON_PA_DE403
!               31003   MOON_ME_DE403
!
! $ Restrictions
!
!     1)  This routine will work correctly if the kernel pool contains
!         no invalid frame specifications. See the description of
!         exception 4 above. Users must ensure that no invalid frame
!         specifications are introduced into the kernel pool, either by
!         loaded kernels or by means of the kernel pool "put" APIs.
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
!-    SPICELIB Version 2.0.0, 08-AUG-2021 (JDR) (NJB)
!
!        Updated to account for switch frame class.
!
!        Edited the header to comply with NAIF standard.
!
!        Updated Example's kernels set to use Clementine PDS archived
!        data.
!
!-    SPICELIB Version 1.1.0, 18-JUN-2015 (NJB)
!
!        Bug fix: previous algorithm failed if the number of frame
!        names fetched from the kernel pool on a single call exceeded
!        twice the kernel variable buffer size. The count of
!        fetched names is now maintained correctly.
!
!-    SPICELIB Version 1.0.0, 22-MAY-2012 (NJB)
!
!-&

! $ Index_Entries
!
!     fetch IDs of reference_frames from the kernel_pool
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           sizei
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: frnmln = 32

      integer(ip), parameter :: kvnmln = 32

      integer(ip), parameter :: bufsiz = 100

!
!     Local variables
!
      character(frnmln)     frname
      character(kvnmln)     kvbuff ( bufsiz )
      character(kvnmln)     kvname
      character(kvnmln)     kvtemp
      character(kvnmln)     kvcode
      character(kvnmln)     kvclas
      character(frnmln)     tmpnam

      integer(ip)           fclass
      integer(ip)           i
      integer(ip)           idcode
      integer(ip)           l
      integer(ip)           m
      integer(ip)           n
      integer(ip)           to
      integer(ip)           total
      integer(ip)           w

      logical               found

      if ( return() ) return

!
!     The output set starts out empty.
!
      call scardi ( 0, idset )

!
!     Check the input frame class.
!
!     This block of code must be kept in sync with frmtyp.inc.
!
      if  (      ( frmcls > swtch ) &
            .or. ( frmcls == 0     ) &
            .or. ( frmcls < all   )  ) then

         call setmsg ( 'Frame class specifier FRMCLS was #; this ' &
         //            'value is not supported.'                  )
         call errint ( '#',  frmcls                               )
         call sigerr ( 'SPICE(BADFRAMECLASS)'                     )
         return

      end if

!
!     Initialize the output buffer index. The
!     index is to be incremented prior to each
!     write to the buffer.
!
      to = 0

!
!     Find all of the kernel variables having names
!     that could correspond to frame name assignments.
!
!     We expect that all frame specifications will
!     include assignments of the form
!
!         FRAME_<ID code>_NAME = <frame name>
!
!     We may pick up some additional assignments that are not part of
!     frame specifications; we plan to filter out as many as possible
!     by looking the corresponding frame ID and frame class
!     assignments.
!
      kvtemp = 'FRAME_*_NAME'

      call gnpool ( kvtemp, 1, bufsiz, n, kvbuff, found )

      total = 0

      do while ( n > 0 )

         total = total + n

!
!        At least one kernel variable was found by the last
!        GNPOOL call. Each of these variables is a possible
!        frame name. Look up each of these candidate names.
!
         do i = 1, n
!
!           Attempt to fetch the right hand side value for
!           the Ith kernel variable found on the previous
!           GNPOOL call.
!
            call gcpool ( kvbuff(i), 1, 1, m, frname, found )

            if ( found ) then
!
!              We found a possible frame name. Attempt to look
!              up an ID code variable for the name. The assignment
!              for the ID code, if present, will have the form
!
!                 FRAME_<name> = <ID code>
!
!              Create the kernel variable name on the left hand
!              side of the assignment.
!
               kvcode = 'FRAME_<name>'

               call repmc ( kvcode, '<name>', frname, kvcode )

!
!              Try to fetch the ID code.
!
               call gipool ( kvcode, 1, 1, l, idcode, found )

               if ( found ) then
!
!                 We found an integer on the right hand side
!                 of the assignment. We probably have a
!                 frame specification at this point. Check that
!                 the variable
!
!                    FRAME_<ID code>_NAME
!
!                 is present in the kernel pool and maps to
!                 the name FRNAME.
!
                  kvname = 'FRAME_<code>_NAME'

                  call repmi ( kvname, '<code>', idcode, kvname )

                  call gcpool ( kvname, 1, 1, w, tmpnam, found )

                  if ( found ) then
!
!                    Try to look up the frame class using a
!                    kernel variable name of the form
!
!                       FRAME_<integer ID code>_CLASS
!
!                    Create the kernel variable name on the left
!                    hand side of the frame class assignment.
!
                     kvclas = 'FRAME_<integer>_CLASS'

                     call repmi ( kvclas, '<integer>', idcode, kvclas )

!
!                    Look for the frame class.
!
                     call gipool ( kvclas, 1, 1, w, fclass, found )

                     if ( .not. found ) then
!
!                       Try to look up the frame class using a kernel
!                       variable name of the form
!
!                          FRAME_<frame name>_CLASS
!
                        kvclas = 'FRAME_<name>_CLASS'

                        call repmc ( kvclas, '<name>', frname, kvclas )

                        call gipool ( kvclas, 1, 1, w, fclass, found )

                     end if

!
!                    At this point FOUND indicates whether we found
!                    the frame class.
!
                     if ( found ) then
!
!                       Check whether the frame class is one
!                       we want.
!
                        if (      ( frmcls == all    ) &
                             .or. ( frmcls == fclass )  ) then
!
!                          We have a winner. Add it to the output set.
!
!                          First make sure the set is large enough to
!                          hold another element.
!
                           if ( to == sizei(idset) ) then

                              call setmsg ( 'Frame ID set argument ' &
                              //            'IDSET has size #; required' &
                              //            ' size is at least #. Make ' &
                              //            'sure that the caller of ' &
                              //            'this routine has initial' &
                              //            'ized IDSET via SSIZEI.'   )
                              call errint ( '#', sizei(idset)          )
                              call errint ( '#', to+1                  )
                              call sigerr ( 'SPICE(SETTOOSMALL)'       )
                              return

                           end if

                           to        = to + 1
                           idset(to) = idcode

                        end if
!
!                       End of IF block for processing a frame having
!                       a frame class matching the request.
!
                     end if
!
!                    End of IF block for finding the frame class.
!
                  end if
!
!                 End of IF block for finding the frame name.
!
               end if
!
!              End of IF block for finding the frame ID.
!
            end if
!
!           End of IF block for finding string value corresponding to
!           the Ith kernel variable matching the name template.
!
         end do
!
!        End of loop for processing last batch of potential
!        frame names.
!
!        Fetch next batch of potential frame names.
!
         call gnpool ( kvtemp, total+1, bufsiz, n, kvbuff, found )

      end do
!
!     At this point all kernel variables that matched the frame name
!     keyword template have been processed. All frames of the specified
!     class or classes have had their ID codes appended to IDSET. In
!     general IDSET is not yet a SPICELIB set, since it's not sorted
!     and it may contain duplicate values.
!
!     Turn IDSET into a set. VALIDI sorts and removes duplicates.
!
      call validi ( sizei(idset), to, idset )

      return
      end
