from client import Client
from agent import Agent
import argparse
import time
import sys

def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(description='Zappy AI Client', add_help=False)
    parser.add_argument('-p', '--port', type=int, default=4242, help='Server port')
    parser.add_argument('-n', '--name', type=str, default='TeamName', help='Team name')  
    parser.add_argument('-h', '--host', type=str, default='127.0.0.1', help='Server hostname')
    parser.add_argument('--help', action='help', help='Show this help message and exit')
    parser.add_argument('--debug', action='store_true', help='Enable debug mode')
    parser.add_argument('--delay', type=float, default=0.1, help='Delay between actions (seconds)')
    
    return parser.parse_args()

def main():
    """Main entry point for the Zappy AI client"""
    args = parse_arguments()
    
    print(f"🤖 Starting Zappy AI Client")
    print(f"├── Team: {args.name}")
    print(f"├── Server: {args.host}:{args.port}")
    print(f"└── Debug: {args.debug}")
    
    client = None
    agent = None
    action_count = 0
    start_time = time.time()
    
    try:
        client = Client(args.name, args.host, args.port)
        client.connect()
        
        agent = Agent(client)
        
        print(f"🎮 Game started!")
        print(f"├── World size: {client.get_world_size()}")
        print(f"└── Client ID: {client.get_client_number()}")
        
        # Main game loop
        while client.is_connected():
            try:
                # Agent thinking cycle
                print(f"\n🧠 Action #{action_count + 1}")
                agent.think()
                
                action_count += 1
                
                # Small delay to avoid overwhelming the server
                time.sleep(args.delay)
                
                # Display statistics every 50 actions
                if action_count % 50 == 0:
                    elapsed = time.time() - start_time
                    print(f"📊 Stats: {action_count} actions in {elapsed:.1f}s ({action_count/elapsed:.1f} actions/s)")
                
            except KeyboardInterrupt:
                print(f"\n⏹️ Interrupted by user")
                break
            except SystemExit as e:
                print(f"\n💀 Agent died: {e}")
                break
            except Exception as e:
                print(f"\n❌ Error in main loop: {e}")
                if args.debug:
                    import traceback
                    traceback.print_exc()
                break
                
    except ConnectionError as e:
        print(f"❌ Connection error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        if args.debug:
            import traceback
            traceback.print_exc()
        sys.exit(1)
    finally:
        # Cleanup
        if client:
            try:
                client.close()
                print(f"🔌 Client disconnected")
            except:
                pass
        
        if action_count > 0:
            elapsed = time.time() - start_time
            print(f"📈 Final stats: {action_count} actions in {elapsed:.1f}s")
        
        print(f"👋 Goodbye!")

if __name__ == "__main__":
    main()