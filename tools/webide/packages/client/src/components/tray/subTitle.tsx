import React, { FC, CSSProperties } from 'react'
import { processPosition, Position } from './position'

type SubtitleProps = {
  position?: Position
}

const Subtitle: FC<SubtitleProps> = ({ children, position }) => {
  const SubtitleStyle: CSSProperties = {
    color: "#686868",
    fontSize: "1.5rem",
    padding: "0",
    margin: "0",
    ...processPosition(position)
  }

  return(
    <h3 style={SubtitleStyle}>
      {children}
    </h3>
  )
}

export default Subtitle
