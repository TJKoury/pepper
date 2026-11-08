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
!
!     Include Section:  EK Column Attribute Descriptor Parameters
!
!        ekattdsc.inc Version 1    23-AUG-1995 (NJB)
!
!
!     This include file declares parameters used in EK column
!     attribute descriptors.  Column attribute descriptors are
!     a simplified version of column descriptors:  attribute
!     descriptors describe attributes of a column but do not contain
!     addresses or pointers.
!
!
!     Size of column attribute descriptor
!
      INTEGER               ADSCSZ
      PARAMETER           ( ADSCSZ = 6 )


!
!     Indices of various pieces of attribute descriptors:
!
!
!     ATTSIZ is the index of the column's class code.  (We use the
!     word `class' to distinguish this item from the column's data
!     type.)
!
      INTEGER               ATTCLS
      PARAMETER           ( ATTCLS = 1 )

!
!     ATTTYP is the index of the column's data type code (CHR, INT, DP,
!     or TIME).  The type is actually implied by the class, but it
!     will frequently be convenient to look up the type directly.
!
!
      INTEGER               ATTTYP
      PARAMETER           ( ATTTYP = ATTCLS + 1 )

!
!     ATTLEN is the index of the column's string length value, if the
!     column has character type.  A value of IFALSE in this element of
!     the descriptor indicates that the strings have variable length.
!
      INTEGER               ATTLEN
      PARAMETER           ( ATTLEN = ATTTYP + 1 )

!
!     ATTSIZ is the index of the column's element size value.  This
!     descriptor element is meaningful for columns with fixed-size
!     entries.  For variable-sized columns, this value is IFALSE.
!
      INTEGER               ATTSIZ
      PARAMETER           ( ATTSIZ = ATTLEN + 1 )

!
!     ATTIDX is the location of a flag that indicates whether the column
!     is indexed.  The flag takes the value ITRUE if the column is
!     indexed and otherwise takes the value IFALSE.
!
      INTEGER               ATTIDX
      PARAMETER           ( ATTIDX = ATTSIZ + 1 )

!
!     ATTNFL is the index of a flag indicating whether nulls are
!     permitted in the column.  The value at location NFLIDX is
!     ITRUE if nulls are permitted and IFALSE otherwise.
!
      INTEGER               ATTNFL
      PARAMETER           ( ATTNFL = ATTIDX + 1 )

!
!     End Include Section:  EK Column Attribute Descriptor Parameters
!
