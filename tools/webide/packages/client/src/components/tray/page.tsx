import React, { FC, Children, ReactElement, useState } from 'react'
import styled from 'styled-components'
import { useMediaQuery }from "react-responsive"
import IconButton from './iconButton'
import Icon from './icon'
import { TrayProvider } from './tray-context'
import Header from './header'
import Sidebar from './sidebar'
import Tray from './tray'

const PageWrapper = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  width: 100%;
  height: 100%;
  padding: 0;
`

const TopNav = styled.div`
  flex-grow: 0;
  flex-shrink: 0;
  display: flex;
  flex-direction: row;
  width:100%;
  height: 64px;
  background-color: #343a40;
`

const Content = styled.div`
  flex-grow: 0;
  flex-shrink: 0;
  width: 100%;
  height: calc(100% - 64px);
  display: flex;
  flex-direction: row;
`

const MainArea = styled.div`
  width: 100%;
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  overflow: auto;
`

const SideNav = styled.div`
  height: 100%;
  flex-basis: 220px;
  flex-grow: 0;
  flex-shrink: 0;
  display: flex;
  flex-direction: column;
  padding: 0;
  background-color: #40484e;
  border-right: 1px solid #292d32;
`

const SideMenuButton = styled(IconButton)`
  background: content-box;
  display: block;
`

type PageLayoutType = FC & {
  Header: FC
  Sidebar: FC
  Tray: FC
}

const Page: PageLayoutType = ({ children }) => {
  const [showSideBar, setShowSideBar] = useState(false)
  const isTabletOrMobile = useMediaQuery({ query: '(max-width: 1024px)' })

  const main: Array<ReactElement<any>> = []
  let header, sidebar, tray

  Children.forEach(children, child => {
    const element = child as ReactElement<any>
    switch (element.type) {
      case Header:
        header = element
        break
      case Sidebar:
        sidebar = element
        break
      case Tray:
        tray = element
        break
      default:
        main.push(element)
    }
  })

  return (
    <TrayProvider>
      {/* <PageWrapper>
        <TopNav>
          {isTabletOrMobile &&
          <SideMenuButton onClick={() => setShowSideBar(!showSideBar)}>
            <Icon.AlignJustify color="white" size={30} />
          </SideMenuButton>
          }
          {header}
        </TopNav> */}

        {/* <Content> */}
        {/* {(!isTabletOrMobile || showSideBar) &&
          <SideNav>
            {sidebar}
          </SideNav>
        } */}
          {/* <MainArea> */}
            {main}
          {/* </MainArea> */}
        {/* </Content> */}
        {tray}
      {/* </PageWrapper> */}
    </TrayProvider>
  )
}
Page.Header = Header
Page.Sidebar = Sidebar
Page.Tray = Tray
export default Page