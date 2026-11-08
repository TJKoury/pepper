!$Procedure ZZDSKSBF ( DSK, manage the API segment buffer )

      SUBROUTINE ZZDSKSBF ( BODYID, NSURF,  SRFLST, ET,  FIXFID, &
                            VERTEX, RAYDIR, POINT,  XPT, HANDLE, &
                            DLADSC, DSKDSC, DC,     IC,  FOUND, &
                            NORMAL                              )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Manage the DSK API segment buffer data structure.
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

      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'zzctr.inc'

      INTEGER               BODYID
      INTEGER               NSURF
      INTEGER               SRFLST ( * )
      DOUBLE PRECISION      ET
      INTEGER               FIXFID
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      INTEGER               HANDLE
      INTEGER               DLADSC ( * )
      DOUBLE PRECISION      DSKDSC ( * )
      DOUBLE PRECISION      DC     ( * )
      INTEGER               IC     ( * )
      LOGICAL               FOUND
      DOUBLE PRECISION      NORMAL ( 3 )

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
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Sizes of source info arrays returned by
!     ZZDSKBUX:
!
      INTEGER               MAXDC
      PARAMETER           ( MAXDC  = 1 )

      INTEGER               MAXIC
      PARAMETER           ( MAXIC  = 1 )


      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 10000 )

      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE = 10 )

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
      INTEGER               BTNBOD
      INTEGER               BTBODY ( BTSIZE )
      INTEGER               BTSEGP ( BTSIZE )
      INTEGER               BTSTSZ ( BTSIZE )

!
!     Segment table variables
!
      INTEGER               STHAN  ( STSIZE )
      DOUBLE PRECISION      STDSCR ( DSKDSZ, STSIZE )
      INTEGER               STDLAD ( DLADSZ, STSIZE )
      INTEGER               STFREE
      DOUBLE PRECISION      STOFF  ( 3,      STSIZE )
      DOUBLE PRECISION      STCTR  ( 3,      STSIZE )
      DOUBLE PRECISION      STRAD  ( STSIZE )


!
!     Local variables
!
      DOUBLE PRECISION      LOCDC  ( MAXDC )

      INTEGER               BIX
      INTEGER               BSRCTR ( CTRSIZ )
      INTEGER               J
      INTEGER               LOCIC  ( MAXIC )
      INTEGER               NSEG
      INTEGER               SEGIDX

      LOGICAL               FIRST
      LOGICAL               UPDATE

!
!     Saved variables
!
      SAVE                  BSRCTR
      SAVE                  BTBODY
      SAVE                  BTNBOD
      SAVE                  BTSEGP
      SAVE                  BTSTSZ
      SAVE                  FIRST
      SAVE                  STCTR
      SAVE                  STDLAD
      SAVE                  STDSCR
      SAVE                  STFREE
      SAVE                  STHAN
      SAVE                  STOFF
      SAVE                  STRAD

!
!     Initial values
!
      DATA                  BTNBOD / 0      /
      DATA                  BTBODY / BTSIZE * 0 /
      DATA                  BSRCTR / CTRSIZ * 0 /
      DATA                  FIRST  / .TRUE. /
      DATA                  STFREE / 1      /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZDSKSBF'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZDSKSBF'          )
      RETURN




!$Procedure ZZSBFXR ( DSK, prepare and perform unprioritized intercept )

      ENTRY ZZSBFXR ( BODYID, NSURF,  SRFLST, ET, &
                      FIXFID, VERTEX, RAYDIR, XPT, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Prepare and execute unprioritized ray-surface intercept
!     computation using DSK API segment buffers.
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


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZSBFXR' )


      IF ( FIRST ) THEN
!
!        Initialize BSR counter.
!
         CALL ZZCTRUIN ( BSRCTR )

         FIRST = .FALSE.

      END IF

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      CALL ZZDSKCHK ( BSRCTR, UPDATE )

      IF ( UPDATE ) THEN
!
!        Make sure the ZZDSKBSR subsystem has completed the segment
!        list for the input body since the last time the BSR loaded
!        kernel state changed.
!
         CALL ZZDSKBBL ( BODYID )

!
!        Initialize the local buffers. We restart from scratch
!        each time the BSR loaded kernel state changes.
!
         CALL ZZDSKSBI ( BTSIZE, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSBFXR' )
         RETURN
      END IF

!
!     Find the index of the input body ID in the body table. If
!     we re-initialized the tables, the index will be zero.
!
      BIX = ISRCHI( BODYID, BTNBOD, BTBODY )


      IF ( BIX .EQ. 0 ) THEN
!
!        We don't have buffered information for this body. Update
!        the body and segment tables to store data for it.
!
         CALL ZZDSKSBA ( BODYID, &
                         BTSIZE, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSBFXR' )
            RETURN
         END IF
!
!        The new body's position in the body table is at the end.
!
         BIX = BTNBOD

      END IF

!
!     Find the ray-surface intercept, using the buffered segment
!     data.
!
      J    = BTSEGP(BIX)
      NSEG = BTSTSZ(BIX)

      CALL ZZDSKBUX ( BODYID,     NSURF,      SRFLST, ET,  FIXFID, &
                      NSEG,       STHAN(J),   STDLAD(1,J), STDSCR(1,J), &
                      STOFF(1,J), STCTR(1,J), STRAD(J),    VERTEX, &
                      RAYDIR,     XPT,        SEGIDX,      LOCDC, &
                      LOCIC,      FOUND                               )

      CALL CHKOUT ( 'ZZSBFXR' )
      RETURN





!$Procedure ZZSBFXRI ( DSK, unprioritized intercept with info )

      ENTRY ZZSBFXRI ( BODYID, NSURF,  SRFLST, ET, &
                       FIXFID, VERTEX, RAYDIR, XPT, HANDLE, &
                       DLADSC, DSKDSC, DC,     IC,  FOUND  )

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


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZSBFXRI' )


      IF ( FIRST ) THEN
!
!        Initialize BSR counter.
!
         CALL ZZCTRUIN ( BSRCTR )

         FIRST = .FALSE.

      END IF

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      CALL ZZDSKCHK ( BSRCTR, UPDATE )

      IF ( UPDATE ) THEN
!
!        Make sure the ZZDSKBSR subsystem has completed the segment
!        list for the input body since the last time the BSR loaded
!        kernel state changed.
!
         CALL ZZDSKBBL ( BODYID )

!
!        Initialize the local buffers. We restart from scratch
!        each time the BSR loaded kernel state changes.
!
         CALL ZZDSKSBI ( BTSIZE, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSBFXRI' )
         RETURN
      END IF

!
!     Find the index of the input body ID in the body table. If
!     we re-initialized the tables, the index will be zero.
!
      BIX = ISRCHI( BODYID, BTNBOD, BTBODY )


      IF ( BIX .EQ. 0 ) THEN
!
!        We don't have buffered information for this body. Update
!        the body and segment tables to store data for it.
!
         CALL ZZDSKSBA ( BODYID, &
                         BTSIZE, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSBFXRI' )
            RETURN
         END IF
!
!        The new body's position in the body table is at the end.
!
         BIX = BTNBOD

      END IF

!
!     Find the ray-surface intercept, using the buffered segment
!     data.
!
      J    = BTSEGP(BIX)
      NSEG = BTSTSZ(BIX)

      CALL ZZDSKBUX ( BODYID,     NSURF,      SRFLST, ET,  FIXFID, &
                      NSEG,       STHAN(J),   STDLAD(1,J), STDSCR(1,J), &
                      STOFF(1,J), STCTR(1,J), STRAD(J),    VERTEX, &
                      RAYDIR,     XPT,        SEGIDX,      DC, &
                      IC,         FOUND                                )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSBFXRI' )
         RETURN
      END IF

      IF ( FOUND ) THEN
!
!        Adjust the segment index to make it relative to the base value
!        1, instead of the current base J.
!
         SEGIDX = SEGIDX + J - 1

         HANDLE = STHAN( SEGIDX )

         CALL MOVEI ( STDLAD(1,SEGIDX), DLADSZ, DLADSC )
         CALL MOVED ( STDSCR(1,SEGIDX), DSKDSZ, DSKDSC )

      END IF

      CALL CHKOUT ( 'ZZSBFXRI' )
      RETURN





!$Procedure ZZSBFNRM ( DSK, prepare and compute unprioritized normal )

      ENTRY ZZSBFNRM ( BODYID, NSURF, SRFLST, ET, &
                       FIXFID, POINT, NORMAL     )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Prepare and execute unprioritized outward surface normal
!     computation using DSK API segment buffers.
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


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSBFNRM' )

!
!     The following block of code is straight cut-and-paste from
!     ZZSBFXR. (We need to consider packaging this code in a
!     utility routine.)
!
      IF ( FIRST ) THEN
!
!        Initialize BSR counter.
!
         CALL ZZCTRUIN ( BSRCTR )

         FIRST = .FALSE.

      END IF

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      CALL ZZDSKCHK ( BSRCTR, UPDATE )

      IF ( UPDATE ) THEN
!
!        Make sure the ZZDSKBSR subsystem has completed the segment
!        list for the input body since the last time the BSR loaded
!        kernel state changed.
!
         CALL ZZDSKBBL ( BODYID )

!
!        Initialize the local buffers. We restart from scratch
!        each time the BSR loaded kernel state changes.
!
         CALL ZZDSKSBI ( BTSIZE, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSBFNRM' )
         RETURN
      END IF

!
!     Find the index of the input body ID in the body table. If
!     we re-initialized the tables, the index will be zero.
!
      BIX = ISRCHI( BODYID, BTNBOD, BTBODY )

      IF ( BIX .EQ. 0 ) THEN
!
!        We don't have buffered information for this body. Update
!        the body and segment tables to store data for it.
!
         CALL ZZDSKSBA ( BODYID, &
                         BTSIZE, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSBFNRM' )
            RETURN
         END IF
!
!        The new body's position in the body table is at the end.
!
         BIX = BTNBOD

      END IF

!
!     Find the outward unit normal vector, using the buffered segment
!     data.
!
      J    = BTSEGP(BIX)
      NSEG = BTSTSZ(BIX)

      CALL ZZDSKBUN ( BODYID,      NSURF,      SRFLST,     ET, &
                      FIXFID,      NSEG,       STHAN(J),   STDLAD(1,J), &
                      STDSCR(1,J), STOFF(1,J), STCTR(1,J), STRAD(J), &
                      POINT,       NORMAL                              )

      CALL CHKOUT ( 'ZZSBFNRM' )
      RETURN

      END

