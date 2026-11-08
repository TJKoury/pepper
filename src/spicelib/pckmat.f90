!$Procedure PCKMAT ( PCK, get transformation matrix at time )

      subroutine pckmat ( body, et, ref, tsipm, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the name of an inertial reference frame and the 6 x 6
!     state transformation matrix from that frame to the body fixed
!     frame of a given body at a specified epoch.
!
! $ Required_Reading
!
!     NAIF_IDS
!     ROTATION
!     TIME
!     PCK
!
! $ Keywords
!
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      integer(ip)          body
      real(wp)             et
      integer(ip)          ref
      real(wp)             tsipm ( 6, 6 )
      logical              found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   ID code of some body.
!     ET         I   Epoch of transformation.
!     REF        O   Integer code for inertial reference frame.
!     TSIPM      O   Transformation from Inertial to PM for BODY at ET.
!     FOUND      O   .TRUE. if data for BODY and ET are found.
!
! $ Detailed_Input
!
!     BODY     is the integer ID code of the body for which the
!              state transformation matrix is requested. Bodies
!              are numbered according to the standard NAIF
!              numbering scheme. The numbering scheme is
!              explained in the NAIF_IDS required reading file.
!
!     ET       is the epoch at which the state transformation
!              matrix is requested.
!
! $ Detailed_Output
!
!     REF      is the integer code for the inertial reference frame
!              of the state transformation matrix TSIPM. (See the
!              routine CHGIRF for a full list of inertial reference
!              frame names.)
!
!     TSIPM    is a 6x6 transformation matrix. It is used to
!              transform states from inertial coordinates to body
!              fixed (also called equator and prime meridian --- PM)
!              coordinates.
!
!              Given a state S in the inertial reference frame
!              specified by REF, the corresponding state in the body
!              fixed reference frame is given by the matrix vector
!              product:
!
!                 TSIPM * S
!
!              See the PCK required reading for further details
!              concerning PCK reference frames.
!
!              NOTE: The inverse of TSIPM is NOT its transpose. The
!              matrix, TSIPM, has the structure shown below:
!
!                          -            -
!                         |       :      |
!                         |   R   :  0   |
!                         | ......:......|
!                         |       :      |
!                         | dR_dt :  R   |
!                         |       :      |
!                          -            -
!
!              where R is a time varying rotation matrix and dR_dt
!              is its derivative. The inverse of this matrix is:
!
!                          -              -
!                         |     T  :       |
!                         |    R   :  0    |
!                         | .......:.......|
!                         |        :       |
!                         |      T :   T   |
!                         | dR_dt  :  R    |
!                         |        :       |
!                          -              -
!
!              The SPICE routine INVSTM is available for producing
!              this inverse.
!
!      FOUND      if the data allowing the computation of a state
!              transformation matrix for the requested time and body
!              are found in a binary PCK file, FOUND will have the
!              value .TRUE., otherwise it will have the value
!              .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the size of the type 20 PCK record to be  retrieved is too
!         large to fit into RECORD, the error SPICE(PCKRECTOOLARGE)
!         is signaled.
!
!     2)  If any issue is detected while reading PCK data, an error is
!         signaled by a routine in the call tree of this routine.
!
!     3)  If the requested transformation matrix cannot be computed
!         using data from loaded binary PCK files, FOUND is returned
!         with the value .FALSE. This is not a SPICE error.
!
! $ Files
!
!     This routine computes transformation matrices using data
!     provided by a loaded binary PCK kernel.
!
! $ Particulars
!
!     The matrix for transforming an inertial state into a body fixed
!     states is the 6x6 matrix shown below as a block structured
!     matrix.
!
!                 -            -
!                |       :      |
!                | TIPM  :  0   |
!                | ......:......|
!                |       :      |
!                | DTIPM : TIPM |
!                |       :      |
!                 -            -
!
!     If a binary PCK file record can be found for the time and body
!     requested, it will be used. The most recently loaded binary PCK
!     file has first priority, followed by previously loaded binary PCK
!     files in backward time order. If no binary PCK file has been
!     loaded, the text P_constants kernel file is used.
!
! $ Examples
!
!     Here we load a binary PCK files and use PCKEUL to get the
!     Euler angles.
!
!     C
!     C  Load binary PCK file.
!     C
!        CALL PCKLOF ('example.pck', HANDLE)
!
!     C  Call routine to get transformation matrix.
!
!        CALL PCKMAT ( BODY, ET, REF, TIPM, FOUND )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 03-JAN-2014 (NJB) (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!        Updated to support type 20. Changed long error message
!        for the case of RECORD having insufficient room: the
!        user is no longer advised to modify the record size.
!
!-    SPICELIB Version 2.0.0, 22-MAR-1995 (KRG) (KSZ)
!
!        Added PCK type 03. Added a new exception. Made some minor
!        comment changes.
!
!-    SPICELIB Version 1.0.0, 21-MAR-1995 (KSZ)
!
!        Replaces PCKEUL and returns the transformation
!        matrix rather than the Euler angles.
!
!-&


! $ Index_Entries
!
!     get state transformation matrix from binary PCK file
!
!-&


!
!     SPICELIB functions
!
      logical              failed
      logical              return

!
!     Local Parameters
!
!     ND and NI values for a PCK file.
!
      integer(ip)           nd
      parameter           ( nd = 2 )

      integer(ip)           ni
      parameter           ( ni = 5 )
!
!     Index for the reference frame code in the integer summary.
!
      integer(ip)           nr
      parameter           ( nr = 2 )
!
!     Length of the descriptor for a PCK file.
!
      integer(ip)           ns
      parameter           ( ns = nd + (ni+1)/2 )
!
!     Index for the data type code in the integer summary.
!
      integer(ip)           nt
      parameter           ( nt = 3 )
!
!     Maximum size allowed for a record in a segment of a binary PCK
!     file.
!
      integer(ip)           maxrec
      parameter           ( maxrec = 130 )
!
!     Number of components in a state vector.
!
      integer(ip)           nstate
      parameter           ( nstate = 6 )

!
!     Local Variables
!
      character*(40)        ident

      real(wp)              dcd    (     nd )
      real(wp)              descr  (     ns )
      real(wp)              estate ( nstate )
      real(wp)              eulang ( nstate )
      real(wp)              record ( maxrec )

      integer(ip)           handle
      integer(ip)           icd    (     ni )
      integer(ip)           recsiz
      integer(ip)           type

!
!     Standard SPICE Error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'PCKMAT' )

!
!     Get a segment applicable to a specified body and epoch.
!
      call pcksfs ( body, et, handle, descr, ident, found )

      if ( failed() ) then
         found = .false.
         call chkout ( 'PCKMAT' )
         return
      end if

      if ( found ) then

!
!        Look at parts of the descriptor.
!
         call dafus ( descr, nd, ni, dcd, icd )

         type = icd( nt )
         ref  = icd( nr )

         if ( type .eq. 02 ) then
!
!           Read in Chebyshev coefficients from segment.
!
            call pckr02 ( handle, descr, et, record )
!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            call pcke02 ( et, record, eulang )

            if ( failed() ) then
               found = .false.
               call chkout ( 'PCKMAT' )
               return
            end if
!
!           From the PCK type two file the Euler angles are
!           retrieved in a particular order.  The routine to
!           get the TSIPM matrix from expects them in another
!           order.  Here we change from EULANG to ESTATE, which
!           has this proper order.
!
            estate (1) = eulang (3)
            estate (2) = eulang (2)
            estate (3) = eulang (1)
            estate (4) = eulang (6)
            estate (5) = eulang (5)
            estate (6) = eulang (4)

!
!           Call routine which takes Euler angles to transformation
!           matrix.
!
            call eul2xf ( estate, 3, 1, 3, tsipm )

            if ( failed() ) then
               found = .false.
               call chkout ( 'PCKMAT' )
               return
            end if


         else if ( type .eq. 03 ) then
!
!           Fetch the number of Chebyshev coefficients, compute the
!           record size needed, and signal an error if there is not
!           enough storage in RECORD. The number of coefficients is the
!           first constant value in the generic segment.
!
            call sgfcon ( handle, descr, 1, 1, record(1) )

            if ( failed() ) then
               found = .false.
               call chkout ( 'PCKMAT' )
               return
            end if

            recsiz = nstate * int(record(1)) + 2

            if ( recsiz .gt. maxrec ) then

               call setmsg ( 'Storage for # double precision numbers' &
               //            ' is needed for a PCK data record and' &
               //            ' only # locations were available.' &
               //            ' Notify the NAIF' &
               //            ' group of this problem.'                 )
               call errint ( '#', recsiz                               )
               call errint ( '#', maxrec                               )
               call sigerr ( 'SPICE(PCKKRECTOOLARGE)'                  )
               call chkout ( 'PCKMAT'                                  )
               return

            end if

            call pckr03 ( handle, descr, et, record )
            call pcke03 ( et, record, tsipm         )

            if ( failed() ) then
               found = .false.
               call chkout ( 'PCKMAT' )
               return
            end if


         else if ( type .eq. 20 ) then
!
!           Read in Chebyshev coefficients from segment.
!
            call pckr20 ( handle, descr, et, record )
!
!           Call evaluation routine to get Euler angles
!           phi, delta, w.
!
            call pcke20 ( et, record, eulang )

            if ( failed() ) then
               found = .false.
               call chkout ( 'PCKMAT' )
               return
            end if
!
!           From the PCK type 20 file the Euler angles are
!           retrieved in a particular order. The routine to
!           get the TSIPM matrix from expects them in another
!           order. Here we change from EULANG to ESTATE, which
!           has this proper order.
!
            estate (1) = eulang (3)
            estate (2) = eulang (2)
            estate (3) = eulang (1)
            estate (4) = eulang (6)
            estate (5) = eulang (5)
            estate (6) = eulang (4)

!
!           Call routine which takes Euler angles to transformation
!           matrix.
!
            call eul2xf ( estate, 3, 1, 3, tsipm )

            if ( failed() ) then
               found = .false.
               call chkout ( 'PCKMAT' )
               return
            end if


         else
!
!           If data matching the requested body and time was not
!           found, FOUND is false.
!
            found = .false.

         end if

      end if

      call chkout ( 'PCKMAT' )
      return
      end
