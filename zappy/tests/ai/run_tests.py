#!/usr/bin/env python3
"""
Test runner script for Zappy AI component
This script demonstrates the AI functionality through comprehensive testing
"""

import sys
import subprocess
import os
from pathlib import Path


def run_command(command, description):
    """Run a command and display results"""
    print(f"\n{'='*60}")
    print(f"🧪 {description}")
    print(f"{'='*60}")
    
    try:
        result = subprocess.run(
            command,
            shell=True,
            capture_output=True,
            text=True,
            timeout=30
        )
        
        if result.returncode == 0:
            print(f"✅ {description} - SUCCESS")
            if result.stdout:
                print("📋 Output:")
                print(result.stdout)
        else:
            print(f"❌ {description} - FAILED")
            if result.stderr:
                print("🚨 Error:")
                print(result.stderr)
                
    except subprocess.TimeoutExpired:
        print(f"⏰ {description} - TIMEOUT")
    except Exception as e:
        print(f"💥 {description} - ERROR: {e}")


def check_dependencies():
    """Check if required dependencies are available"""
    print("🔍 Checking dependencies...")
    
    # Check Python
    try:
        import sys
        print(f"✅ Python {sys.version}")
    except:
        print("❌ Python not available")
        return False
    
    # Check if test files exist
    test_files = [
        "test_agent.py",
        "test_decision_logic.py", 
        "test_game_state.py",
        "test_parsers.py"
    ]
    
    missing_files = []
    for test_file in test_files:
        if not Path(test_file).exists():
            missing_files.append(test_file)
    
    if missing_files:
        print(f"❌ Missing test files: {missing_files}")
        return False
    
    print("✅ All test files present")
    return True


def install_dependencies():
    """Install test dependencies"""
    print("\n📦 Installing test dependencies...")
    
    try:
        # Try to install pytest and dependencies
        subprocess.run([
            sys.executable, "-m", "pip", "install", "-r", "requirements-test.txt"
        ], check=True, capture_output=True)
        print("✅ Dependencies installed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ Failed to install dependencies: {e}")
        print("💡 Try running: pip install pytest pytest-mock pytest-cov")
        return False


def run_specific_tests():
    """Run specific test categories"""
    
    test_categories = [
        ("test_agent.py", "Agent Core Functionality Tests"),
        ("test_decision_logic.py", "Decision Making Logic Tests"),
        ("test_game_state.py", "Game State Management Tests"),
        ("test_parsers.py", "Parser Functions Tests")
    ]
    
    for test_file, description in test_categories:
        if Path(test_file).exists():
            run_command(
                f"python -m pytest {test_file} -v",
                description
            )


def main():
    """Main test runner function"""
    print("🤖 Zappy AI Test Suite")
    print("=" * 60)
    
    # Check if we're in the right directory
    if not Path("agent.py").exists():
        print("❌ Error: agent.py not found. Please run from the ai/ directory")
        sys.exit(1)
    
    # Check dependencies
    if not check_dependencies():
        sys.exit(1)
    
    # Try to install dependencies
    if not install_dependencies():
        print("⚠️  Continuing without installing dependencies...")
    
    # Run all tests
    run_command(
        "python -m pytest test_*.py -v --tb=short",
        "Complete Test Suite"
    )
    
    # Run specific test categories
    run_specific_tests()
    
    # Try to run with coverage if available
    run_command(
        "python -m pytest test_*.py --cov=. --cov-report=term-missing",
        "Coverage Report"
    )
    
    print("\n" + "=" * 60)
    print("🎯 Test Summary")
    print("=" * 60)
    print("✅ Tests completed!")
    print("📊 Check the output above for detailed results")
    print("💡 To run tests manually: python -m pytest test_*.py -v")
    print("🔍 To run with coverage: python -m pytest test_*.py --cov=.")


if __name__ == "__main__":
    main()