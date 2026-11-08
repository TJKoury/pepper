!$Procedure ZZDSKSBF ( DSK, manage the API segment buffer )

      subroutine zzdsksbf ( bodyid, nsurf,  srflst, et,  fixfid, &
                            vertex, raydir, point,  xpt, handle, &
                            dladsc, dskdsc, dc,     ic,  found, &
                            normal                              )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Manage the DSK API segment buffer data structure.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      implicit none

      include 'dla.inc'
      include 'dskdsc.inc'
      include 'zzctr.inc'

      integer(ip)           bodyid
      integer(ip)           nsurf
      integer(ip)           srflst ( * )
      real(wp)              et
      integer(ip)           fixfid
      real(wp)              vertex ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              point  ( 3 )
      real(wp)              xpt    ( 3 )
      integer(ip)           handle
      integer(ip)           dladsc ( * )
      real(wp)              dskdsc ( * )
      real(wp)              dc     ( * )
      integer(ip)           ic     ( * )
      logical               found
      real(wp)              normal ( 3 )

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     BODYID     I   ZZSBFXR, ZZSBFNRM
!     NSURF      I   ZZSBFXR, ZZSBFNRM
!     SRFLST     I   ZZSBFXR, ZZSBFNRM
!     ET         I   ZZSBFXR, ZZSBFNRM
!     FIXFID     I   ZZSBFXR, ZZSBFNRM
!     VERTEX     I   ZZSBFXR
!     RAYDIR     I   ZZSBFXR
!     POINT      I   ZZSBFNRM
!     XPT        O   ZZSBFXR
!     FOUND      O   ZZSBFXR
!     HANDLE     O   ZZSBFXRI
!     DLADSC     O   ZZSBFXRI
!     DSKDSC     O   ZZSBFXRI
!     DC         O   ZZSBFXRI
!     IC         O   ZZSRFXRI
!     NORMAL     O   ZZSBFNRM
!
! $ Detailed_Input
!
!     See the entry points.
!
! $ Detailed_Output
!
!     See the entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, it signals the error
!         SPICE(BOGUSENTRY).
!
!     See the entry points for descriptions of errors specific to
!     those routines.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!
!     Data structure management
!     =========================
!
!     This routine manages data structures used by SPICELIB geometry
!     APIs to perform efficient computations using DSK data. This is
!     the umbrella routine in which these structures are declared.
!
!     This routine also contains entry points that use these data
!     structures to perform geometric computations using DSK data.
!     See the section titled "Computations" below.
!
!     In a sense, ZZDSKSBF sits "above" the ZZDSKBSR subsystem.
!     High-level geometry routines access the data stored in the
!     ZZDSKSBF arrays directly; they don't call entry points of
!     ZZDSKBSR.
!
!     The ZZDSKSBF subsystem maintains two logical tables: a body
!     table and a segment table. Both tables are designed to store
!     a fixed maximum number of items; there is no "virtual storage"
!     concept that applies.
!
!     Items in both tables have priority assigned on a first-in,
!     first-out basis. When it becomes necessary to remove an
!     item to make room for another, the lowest-indexed item is
!     removed first, and the tables are compressed so that the
!     remaining items are contiguous.
!
!     When the state of the underlying ZZDSKBSR system changes due to
!     loading or unloading of DSK files, the ZZDSKSBF body and segment
!     tables must be re-initialized. Entry points of ZZDSKSBF
!     perform this action by calling ZZDSKSBI.
!
!     In addition to the tables maintained by this subsystem, a
!     counter is maintained. This counter is used to determine
!     whether the locally buffered data are in sync with the
!     information stored in the ZZDSKBSR subsystem.
!
!
!     Computations
!     ============
!
!     This routine contains the following entry points that use
!     buffered segment data to perform computations:
!
!        ZZSBFXR:    prepare for and compute unprioritized
!                    ray-surface intercept using DSK data.
!
!        ZZSBFXRI:   prepare for and compute unprioritized
!                    ray-surface intercept using DSK data; return data
!                    source information such as file handle, DLA
!                    descriptor, and plate ID as well.
!
!        ZZSBFNRM:   prepare for and compute outward normal
!                    vector at a specified surface point,
!                    using unprioritized DSK data.
!
! $ Examples
!
!     See usage in ZZDSKSBF.
!
! $ Restrictions
!
!     1) This is a private routine.
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
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED calls in each entry point.
!
!        17-MAY-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     manage DSK API segment buffer
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           isrchi

      logical               failed
      logical               return

!
!     Local parameters
!

!
!     Sizes of source info arrays returned by
!     ZZDSKBUX:
!
      integer(ip)           maxdc
      parameter           ( maxdc  = 1 )

      integer(ip)           maxic
      parameter           ( maxic  = 1 )


      integer(ip)           stsize
      parameter           ( stsize = 10000 )

      integer(ip)           btsize
      parameter           ( btsize = 10 )

!
!     Body table variables
!     --------------------
!
!        BTNBOD  is the number of bodies in the body table.
!
!        BTBODY  is an array of body ID codes.
!
!        BTSEGP  is an array of pointers (start indices) to entries in
!                the segment table. The Ith pointer indicates the start
!                index for entries for the Ith body.
!
!        BTSTSZ  is an array of segment table entry counts. The Ith
!                element of BTSTSZ is the number of entries in the
!                segment table for the Ith body.
!
!
      integer(ip)           btnbod
      integer(ip)           btbody ( btsize )
      integer(ip)           btsegp ( btsize )
      integer(ip)           btstsz ( btsize )

!
!     Segment table variables
!
      integer(ip)           sthan  ( stsize )
      real(wp)              stdscr ( dskdsz, stsize )
      integer(ip)           stdlad ( dladsz, stsize )
      integer(ip)           stfree
      real(wp)              stoff  ( 3,      stsize )
      real(wp)              stctr  ( 3,      stsize )
      real(wp)              strad  ( stsize )


!
!     Local variables
!
      real(wp)              locdc  ( maxdc )

      integer(ip)           bix
      integer(ip)           bsrctr ( ctrsiz )
      integer(ip)           j
      integer(ip)           locic  ( maxic )
      integer(ip)           nseg
      integer(ip)           segidx

      logical               first
      logical               update

!
!     Saved variables
!
      save                  bsrctr
      save                  btbody
      save                  btnbod
      save                  btsegp
      save                  btstsz
      save                  first
      save                  stctr
      save                  stdlad
      save                  stdscr
      save                  stfree
      save                  sthan
      save                  stoff
      save                  strad

!
!     Initial values
!
      data                  btnbod / 0      /
      data                  btbody / btsize * 0 /
      data                  bsrctr / ctrsiz * 0 /
      data                  first  / .true. /
      data                  stfree / 1      /



      if ( return() ) then
         return
      end if

      call chkin  ( 'ZZDSKSBF'          )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZDSKSBF'          )
      return




!$Procedure ZZSBFXR ( DSK, prepare and perform unprioritized intercept )

      entry zzsbfxr ( bodyid, nsurf,  srflst, et, &
                      fixfid, vertex, raydir, xpt, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Prepare and execute unprioritized ray-surface intercept
!     computation using DSK API segment buffers.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               NSURF
!     INTEGER               SRFLST ( * )
!     DOUBLE PRECISION      ET
!     INTEGER               FIXFID
!     DOUBLE PRECISION      VERTEX ( 3 )
!     DOUBLE PRECISION      RAYDIR ( 3 )
!     DOUBLE PRECISION      XPT    ( 3 )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     NSURF      I   Number of surface IDs in list.
!     SRFLST     I   Surface ID list.
!     ET         I   Evaluation epoch, seconds past J2000 TDB.
!     FIXFID     I   Frame ID of ray and intercept.
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     XPT        O   Surface intercept point.
!     FOUND      O   Found flag. True if intercept exists.
!
! $ Detailed_Input
!
!     BODYID     is the ID code of a target body. The ray-surface
!                intercept computation is performed using data
!                that represent the surface of this body.
!
!     NSURF,
!     SRFLST     are, respectively, a count of surface IDs and
!                a list of IDs. If the list is non-empty, only
!                the indicated surfaces will be used. If the
!                list is empty, all surfaces associated with the
!                input body ID will be considered.
!
!     ET         is the epoch for which the computation is to be
!                performed. This epoch is used for DSK segment
!                selection; only segments containing ET in their time
!                coverage interval will be used. ET is expressed as
!                seconds past J2000 TDB.
!
!     FIXFID     is the frame ID of a body-fixed frame associated
!                with the body designated by BODYID. This frame
!                is used to express the input ray's vertex and
!                direction vector. The output intercept will be
!                expressed in this frame as well.
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector
!                of a ray. Both vectors are expressed in the frame
!                designated by FIXFID.
!
! $ Detailed_Output
!
!     XPT        is the surface intercept on the target body
!                nearest to the ray's vertex, if the intercept
!                exists.
!
!     FOUND      is a logical flag that is set to .TRUE. if and
!                only if an intercept exists.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, it signals the error
!         SPICE(BOGUSENTRY).
!
!     See the entry points for descriptions of errors specific to
!     those routines.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine prepares the local buffers for a ray-surface
!     intercept computation using unprioritized DSK data.
!     It calls ZZDSKBUX to perform the computation.
!
! $ Examples
!
!     See usage in ZZRAYSFX.
!
! $ Restrictions
!
!     1) This is a private routine.
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
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED calls.
!
!        12-MAY-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     prepare unprioritized ray surface intercept
!
!-&


      if ( return() ) then
         return
      end if

      call chkin  ( 'ZZSBFXR' )


      if ( first ) then
!
!        Initialize BSR counter.
!
         call zzctruin ( bsrctr )

         first = .false.

      end if

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      call zzdskchk ( bsrctr, update )

      if ( update ) then
!
!        Make sure the ZZDSKBSR subsystem has completed the segment
!        list for the input body since the last time the BSR loaded
!        kernel state changed.
!
         call zzdskbbl ( bodyid )

!
!        Initialize the local buffers. We restart from scratch
!        each time the BSR loaded kernel state changes.
!
         call zzdsksbi ( btsize, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )
      end if

      if ( failed() ) then
         call chkout ( 'ZZSBFXR' )
         return
      end if

!
!     Find the index of the input body ID in the body table. If
!     we re-initialized the tables, the index will be zero.
!
      bix = isrchi( bodyid, btnbod, btbody )


      if ( bix .eq. 0 ) then
!
!        We don't have buffered information for this body. Update
!        the body and segment tables to store data for it.
!
         call zzdsksba ( bodyid, &
                         btsize, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )

         if ( failed() ) then
            call chkout ( 'ZZSBFXR' )
            return
         end if
!
!        The new body's position in the body table is at the end.
!
         bix = btnbod

      end if

!
!     Find the ray-surface intercept, using the buffered segment
!     data.
!
      j    = btsegp(bix)
      nseg = btstsz(bix)

      call zzdskbux ( bodyid,     nsurf,      srflst, et,  fixfid, &
                      nseg,       sthan(j),   stdlad(1,j), stdscr(1,j), &
                      stoff(1,j), stctr(1,j), strad(j),    vertex, &
                      raydir,     xpt,        segidx,      locdc, &
                      locic,      found                               )

      call chkout ( 'ZZSBFXR' )
      return





!$Procedure ZZSBFXRI ( DSK, unprioritized intercept with info )

      entry zzsbfxri ( bodyid, nsurf,  srflst, et, &
                       fixfid, vertex, raydir, xpt, handle, &
                       dladsc, dskdsc, dc,     ic,  found  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Prepare and execute unprioritized ray-surface intercept
!     computation using DSK API segment buffers. Return source
!     information including handle, DLA descriptor, and
!     segment-specific information such as plate ID.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               NSURF
!     INTEGER               SRFLST ( * )
!     DOUBLE PRECISION      ET
!     INTEGER               FIXFID
!     DOUBLE PRECISION      VERTEX ( 3 )
!     DOUBLE PRECISION      RAYDIR ( 3 )
!     DOUBLE PRECISION      XPT    ( 3 )
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!     DOUBLE PRECISION      DC     ( * )
!     INTEGER               IC     ( * )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     NSURF      I   Number of surface IDs in list.
!     SRFLST     I   Surface ID list.
!     ET         I   Evaluation epoch, seconds past J2000 TDB.
!     FIXFID     I   Frame ID of ray and intercept.
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     XPT        O   Surface intercept point.
!     HANDLE     O   Handle of DSK file.
!     DLADSC     O   DLA descriptor of segment.
!     DSKDSC     O   DSK descriptor of segment.
!     DC         O   Double precision component of source info.
!     IC         O   Integer component of source info.
!     FOUND      O   Found flag. True if intercept exists.
!
! $ Detailed_Input
!
!     BODYID     is the ID code of a target body. The ray-surface
!                intercept computation is performed using data
!                that represent the surface of this body.
!
!     NSURF,
!     SRFLST     are, respectively, a count of surface IDs and
!                a list of IDs. If the list is non-empty, only
!                the indicated surfaces will be used. If the
!                list is empty, all surfaces associated with the
!                input body ID will be considered.
!
!     ET         is the epoch for which the computation is to be
!                performed. This epoch is used for DSK segment
!                selection; only segments containing ET in their time
!                coverage interval will be used. ET is expressed as
!                seconds past J2000 TDB.
!
!     FIXFID     is the frame ID of a body-fixed frame associated
!                with the body designated by BODYID. This frame
!                is used to express the input ray's vertex and
!                direction vector. The output intercept will be
!                expressed in this frame as well.
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector
!                of a ray. Both vectors are expressed in the frame
!                designated by FIXFID.
!
! $ Detailed_Output
!
!     XPT        is the surface intercept on the target body
!                nearest to the ray's vertex, if the intercept
!                exists.
!
!     HANDLE     is the handle of the DSK file that contributed
!                the segment in which the intercept was found.
!
!     DLADSC     is the DLA descriptor of the segment in which
!                the intercept was found.
!
!     DSKDSC     is the DSK descriptor of the segment in which
!                the intercept was found.
!
!     DC         is the double precision component of the source
!                data associated with the intercept.
!
!     IC         is the integer component of the source
!                data associated with the intercept.
!
!                   For type 2 segments, this component contains
!                   a plate ID in the first element.
!
!     FOUND      is a logical flag that is set to .TRUE. if and
!                only if an intercept exists. The other outputs
!                are valid if and only if FOUND is .TRUE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, it signals the error
!         SPICE(BOGUSENTRY).
!
!     See the entry points for descriptions of errors specific to
!     those routines.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine prepares the local buffers for a ray-surface
!     intercept computation using unprioritized DSK data.
!     It calls ZZDSKBUX to perform the computation.
!
! $ Examples
!
!     See usage in ZZRAYSFX.
!
! $ Restrictions
!
!     1) This is a private routine.
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
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED calls.
!
!        12-MAY-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     unprioritized ray surface intercept with info
!
!-&


      if ( return() ) then
         return
      end if

      call chkin  ( 'ZZSBFXRI' )


      if ( first ) then
!
!        Initialize BSR counter.
!
         call zzctruin ( bsrctr )

         first = .false.

      end if

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      call zzdskchk ( bsrctr, update )

      if ( update ) then
!
!        Make sure the ZZDSKBSR subsystem has completed the segment
!        list for the input body since the last time the BSR loaded
!        kernel state changed.
!
         call zzdskbbl ( bodyid )

!
!        Initialize the local buffers. We restart from scratch
!        each time the BSR loaded kernel state changes.
!
         call zzdsksbi ( btsize, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )
      end if

      if ( failed() ) then
         call chkout ( 'ZZSBFXRI' )
         return
      end if

!
!     Find the index of the input body ID in the body table. If
!     we re-initialized the tables, the index will be zero.
!
      bix = isrchi( bodyid, btnbod, btbody )


      if ( bix .eq. 0 ) then
!
!        We don't have buffered information for this body. Update
!        the body and segment tables to store data for it.
!
         call zzdsksba ( bodyid, &
                         btsize, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )

         if ( failed() ) then
            call chkout ( 'ZZSBFXRI' )
            return
         end if
!
!        The new body's position in the body table is at the end.
!
         bix = btnbod

      end if

!
!     Find the ray-surface intercept, using the buffered segment
!     data.
!
      j    = btsegp(bix)
      nseg = btstsz(bix)

      call zzdskbux ( bodyid,     nsurf,      srflst, et,  fixfid, &
                      nseg,       sthan(j),   stdlad(1,j), stdscr(1,j), &
                      stoff(1,j), stctr(1,j), strad(j),    vertex, &
                      raydir,     xpt,        segidx,      dc, &
                      ic,         found                                )

      if ( failed() ) then
         call chkout ( 'ZZSBFXRI' )
         return
      end if

      if ( found ) then
!
!        Adjust the segment index to make it relative to the base value
!        1, instead of the current base J.
!
         segidx = segidx + j - 1

         handle = sthan( segidx )

         call movei ( stdlad(1,segidx), dladsz, dladsc )
         call moved ( stdscr(1,segidx), dskdsz, dskdsc )

      end if

      call chkout ( 'ZZSBFXRI' )
      return





!$Procedure ZZSBFNRM ( DSK, prepare and compute unprioritized normal )

      entry zzsbfnrm ( bodyid, nsurf, srflst, et, &
                       fixfid, point, normal     )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Prepare and execute unprioritized outward surface normal
!     computation using DSK API segment buffers.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               NSURF
!     INTEGER               SRFLST ( * )
!     DOUBLE PRECISION      ET
!     INTEGER               FIXFID
!     DOUBLE PRECISION      POINT  ( 3 )
!     DOUBLE PRECISION      NORMAL ( 3 )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     NSURF      I   Number of surface IDs in list.
!     SRFLST     I   Surface ID list.
!     ET         I   Evaluation epoch, seconds past J2000 TDB.
!     FIXFID     I   Frame ID of surface point and normal vector.
!     POINT      I   Surface point.
!     NORMAL     O   Unit length outward normal at input point.
!
! $ Detailed_Input
!
!     BODYID     is the ID code of a target body. The outward
!                normal vector computation is performed using data
!                that represent the surface of this body.
!
!     NSURF,
!     SRFLST     are, respectively, a count of surface IDs and
!                a list of IDs. If the list is non-empty, only
!                the indicated surfaces will be used. If the
!                list is empty, all surfaces associated with the
!                input body ID will be considered.
!
!     ET         is the epoch for which the computation is to be
!                performed. This epoch is used for DSK segment
!                selection; only segments containing ET in their time
!                coverage interval will be used. ET is expressed as
!                seconds past J2000 TDB.
!
!     FIXFID     is the frame ID of a body-fixed frame associated with
!                the body designated by BODYID. This frame is used to
!                express the input surface point. The output normal
!                vector will be expressed in this frame as well.
!
!     POINT      is a surface point on the body designated by BODYID.
!                POINT is not arbitrary: it must be within a small
!                tolerance of the actual surface. Usually POINT is
!                obtained as the output of a SPICE computation.
!
!                POINT is expressed in the frame designated by FIXFID.
!
! $ Detailed_Output
!
!     NORMAL     is the unit-length outward surface normal vector at
!                the input POINT.
!
!                NORMAL is expressed in the frame designated by FIXFID.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, it signals the error
!         SPICE(BOGUSENTRY).
!
!     See the entry points for descriptions of errors specific to
!     those routines.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine prepares the local buffers for a surface normal
!     vector computation using unprioritized DSK data. It calls
!     ZZDSKBUN to perform the computation.
!
! $ Examples
!
!     See usage in DSKNRM (provisional name).
!
! $ Restrictions
!
!     1) This is a private routine.
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
!-    SPICELIB Version 1.0.0, 22-FEB-2017 (NJB)
!
!        Added FAILED calls.
!
!        12-MAY-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     prepare unprioritized normal vector computation
!
!-&


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZSBFNRM' )

!
!     The following block of code is straight cut-and-paste from
!     ZZSBFXR. (We need to consider packaging this code in a
!     utility routine.)
!
      if ( first ) then
!
!        Initialize BSR counter.
!
         call zzctruin ( bsrctr )

         first = .false.

      end if

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      call zzdskchk ( bsrctr, update )

      if ( update ) then
!
!        Make sure the ZZDSKBSR subsystem has completed the segment
!        list for the input body since the last time the BSR loaded
!        kernel state changed.
!
         call zzdskbbl ( bodyid )

!
!        Initialize the local buffers. We restart from scratch
!        each time the BSR loaded kernel state changes.
!
         call zzdsksbi ( btsize, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )
      end if

      if ( failed() ) then
         call chkout ( 'ZZSBFNRM' )
         return
      end if

!
!     Find the index of the input body ID in the body table. If
!     we re-initialized the tables, the index will be zero.
!
      bix = isrchi( bodyid, btnbod, btbody )

      if ( bix .eq. 0 ) then
!
!        We don't have buffered information for this body. Update
!        the body and segment tables to store data for it.
!
         call zzdsksba ( bodyid, &
                         btsize, stsize, btbody, btnbod, btsegp, &
                         btstsz, sthan,  stdscr, stdlad, stfree, &
                         stoff,  stctr,  strad                  )

         if ( failed() ) then
            call chkout ( 'ZZSBFNRM' )
            return
         end if
!
!        The new body's position in the body table is at the end.
!
         bix = btnbod

      end if

!
!     Find the outward unit normal vector, using the buffered segment
!     data.
!
      j    = btsegp(bix)
      nseg = btstsz(bix)

      call zzdskbun ( bodyid,      nsurf,      srflst,     et, &
                      fixfid,      nseg,       sthan(j),   stdlad(1,j), &
                      stdscr(1,j), stoff(1,j), stctr(1,j), strad(j), &
                      point,       normal                              )

      call chkout ( 'ZZSBFNRM' )
      return

      end

