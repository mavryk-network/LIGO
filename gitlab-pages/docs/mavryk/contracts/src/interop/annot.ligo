type animal is
| [@annot memory] Elephant of unit
| [@annot face] Dog of unit
| [@annot fish] Cat of unit
type artist is record [
  [@annot style] genre : string;
  [@annot from] since : timestamp;
  [@annot performer] name : string
]