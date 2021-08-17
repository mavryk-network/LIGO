import React, { createContext, useContext, Component } from 'react'

type Size = "lg" | "sm" | "large" | "small"

interface TrayInterface {
  isTrayOpen: boolean
  trayTitle: string
  trayContent: any
  traySize: Size
  openTray: (content: any, title?: string, size?: Size) => void
  closeTray: () => void
  updateTray: (contentProps: any) => void
}

const TrayContext = createContext<Partial<TrayInterface>>({})

export const useTray = () => {
  const context = useContext(TrayContext)
  if (!context) {
    throw new Error(`useTray must be used within a TrayProvider`)
  }

  return context as TrayInterface
}

export class TrayProvider extends Component {
  state = {
    isOpen: false,
    content: null,
    title: "",
    size: "sm" as Size
  }

  closeKeypress = (e: KeyboardEvent) => {
    // Esc key == 27
    if (e.keyCode === 27) {
      this.closeTray()
    }
  }

  openTray = (content: any, title = "", size: Size = "sm") => {
    document.addEventListener('keydown', this.closeKeypress)
    this.setState({ isOpen: true, content, title, size })
  }

  removeCloseEventListener = () => {
    document.removeEventListener('keydown', this.closeKeypress)
  }

  updateTray = (content: any) => {
    this.setState({ content })
  }

  closeTray = () => {
    this.setState({ isOpen: false, content: null })
    this.removeCloseEventListener()
  }

  render() {
    const { children } = this.props
    const { isOpen, content, title, size } = this.state

    const value = {
      isTrayOpen: isOpen,
      trayTitle: title,
      trayContent: content,
      traySize: size,
      openTray: this.openTray,
      closeTray: this.closeTray,
      updateTray: this.updateTray,
    }

    return (
      <TrayContext.Provider value={value}>
        <>{children}</>
      </TrayContext.Provider>
    )
  }
}
