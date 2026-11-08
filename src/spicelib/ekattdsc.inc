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
