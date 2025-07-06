'use client';

import React from 'react';
import { cn } from '@/lib/utils';

interface CodePreviewProps {
  content: string;
  language: string;
  className?: string;
}

export default function CodePreview({ content, language, className }: CodePreviewProps) {
  // Simple formatter for JSON display
  const formattedContent = language === 'json' ? formatJson(content) : content;

  return (
    <div className={cn("overflow-auto bg-muted", className)}>
      <pre className="p-4 text-sm">
        <code className="font-mono">{formattedContent}</code>
      </pre>
    </div>
  );
}

// Simple JSON formatter for better display
function formatJson(jsonString: string): string {
  try {
    const parsed = JSON.parse(jsonString);
    return JSON.stringify(parsed, null, 2);
  } catch (e) {
    // If parsing fails, return the original string
    return jsonString;
  }
}