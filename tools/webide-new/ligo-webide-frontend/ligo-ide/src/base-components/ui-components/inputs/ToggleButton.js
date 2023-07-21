import Wifi from "@mui/icons-material/Wifi";
import WifiOff from "@mui/icons-material/WifiOff";
import ToggleButton from "@mui/material/ToggleButton";
import React, { useState } from "react";

export default function ({ offStateLabel, onStateLabel, selected, onChange }) {
  let contents;
  if (selected) {
    contents = (
      <>
        {" "}
        <Wifi />
        {onStateLabel}{" "}
      </>
    );
  } else {
    contents = (
      <>
        {" "}
        <WifiOff /> {offStateLabel}{" "}
      </>
    );
  }
  return (
    <ToggleButton
      sx={{
        cursor: "pointer",
        paddingLeft: "1rem",
        paddingRight: "1rem",
        paddingTop: 0,
        paddingBottom: 0,
        border: "none",
      }}
      value={selected ? "check" : ""}
      selected={selected}
      onChange={onChange}
    >
      {contents}
    </ToggleButton>
  );
}
