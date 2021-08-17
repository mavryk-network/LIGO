import { CSSProperties } from 'react';

export type Position = 'left' | 'center' | 'right';

export const processPosition = (position: Position = 'left'): CSSProperties => {
  switch (position) {
    case 'left':
      return { alignSelf: 'flex-start' };
    case 'right':
      return { alignSelf: 'flex-end' };
    case 'center':
      return { alignSelf: 'center' };
    default:
      return {};
  }
};
