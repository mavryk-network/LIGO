// This is interpret.ligo
type myDataType is map (int, string)

function encodeEntry (const a : int; const b : string) : myDataType is
  Map.literal (list [(a, b)])