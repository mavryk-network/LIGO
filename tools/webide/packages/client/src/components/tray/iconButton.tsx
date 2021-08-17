import styled from 'styled-components'

const IconButton = styled.button<{float?: 'right' | 'left'}>`
  border: none;
  flex: inherit;
  display: block;
  float: ${({float}) => float ? 'right' : 'left'};

  :focus {
    outline: none;
  }
`

export default IconButton