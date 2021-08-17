import React, { FC, Children, PropsWithChildren } from 'react'
import styled from 'styled-components'

const Wrapper = styled.div<{align: "left"|"right"}>`
  width: 100%;
  display: flex;
  justify-content: ${props => props.align === "left" ? 'flex-start' : 'flex-end'};
`

const FirstElement = styled.div`
  margin-left: 0;
  margin-right: 5px;
`

const MiddleElement = styled.div`
  margin-left: 5px;
  margin-right: 5px;
`

const LastElement = styled.div`
  margin-left: 5px;
  margin-right: 0;
`

interface Props {
  align?: "left"|"right"
}

export const SpacedBox: FC<PropsWithChildren<Props>> = ({ children, align = "left" }) => {
  const [first, ...rest] = Children.toArray(children)
  const last = rest.pop()

  return (
    <Wrapper align={align}>
      <FirstElement>{first}</FirstElement>
      {rest.map((element, i) =>
        <MiddleElement key={`part_${i}`}>{element}</MiddleElement>
      )}
      <LastElement>{last}</LastElement>
    </Wrapper>
  )
}
